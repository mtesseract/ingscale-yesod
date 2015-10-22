module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Ingscale
import qualified Network.Wai as Wai
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.List as L

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Ingscale"
--    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            document.getElementsByName('mode1Spec')[0].disabled = true;
            document.getElementsByName('mode2Spec')[0].disabled = true;
            function scale() {
                document.getElementById("ingredientsScaled").value = "";
                var ingredients = document.getElementById("ingredients");
                var ingredientsScaled = document.getElementById("ingredientsScaled");
                var mode1 = document.getElementById("mode1");
                var mode2 = document.getElementById("mode2");
                var mode1Spec = document.getElementById("mode1Spec");
                var mode2Spec = document.getElementById("mode2Spec");
                var request = "";
                var xhr = new XMLHttpRequest();
                xhr.open("POST", "@{HomeR}", true);
                xhr.setRequestHeader("Content-type", "text/plain; charset=utf-8")
                xhr.onreadystatechange = function() {
                  if(this.readyState == 4 && this.status == 200) {
                    ingredientsScaled.value = this.responseText;
                  }
                }
                if (mode1.checked) {
                    request = mode1Spec.value + "\n";
                } else if (mode2.checked) {
                    request = mode2Spec.value + "\n";
                }
                request = request + ingredients.value;
                xhr.send(request);
            }
        |]
    toWidgetBody
        [hamlet|
<h1>Ingscale

<form>
 <table style="width:100%">
  <tr>
   <td>
    <h3>1st step
    Enter ingredient list.
   <td>
    <h3>2nd step
    Select scaling mode.
   <td>
    <h3>3rd step 
    Specify desired scaling.
   <td>
    <h3>4th step
    Retrieve scaled ingredient list.

  <tr>
   <td>
    <textarea id="ingredients" name="ingredients" rows="10" cols="28" placeholder="Chocolate, 1 2/3 tbsp">
   <td>
    <input type="radio" id="mode1" name="mode" value="1" onclick="document.getElementsByName('mode1Spec')[0].disabled = false; document.getElementsByName('mode2Spec')[0].disabled = true;"> Scale by number
    <br>
    <input type="radio" id="mode2" name="mode" value="2" onclick="document.getElementsByName('mode1Spec')[0].disabled = true; document.getElementsByName('mode2Spec')[0].disabled = false;"> Scale to ingredient
   <td>
    Scale by this number:<br>
    <input type="text" name="mode1Spec" id="mode1Spec" placeholder="1.7">
    <br>
    Scale to this ingredient quantity:<br>
    <input type="text" name="mode2Spec" id="mode2Spec" placeholder="Chocolate, 2 tbsp">
    <br>
    <button .btn .btn-primary type="button" onclick="scale();">
       Scale Ingredients <span class="glyphicon glyphicon-upload"></span>
   <td>
    <textarea id="ingredientsScaled" rows="10" cols="60" readonly>
        |]

postHomeR :: Handler Text
postHomeR = do
  wr <- waiRequest
  x <- liftIO $ Wai.requestBody wr
  let x' = TE.decodeUtf8 x
  let bodyLines = T.lines x'
  if length bodyLines < 2
    then invalidArgs ["Invalid Input"]
    else let spec = L.head bodyLines
             spec' = map (T.unpack . T.strip) $ T.split (==',') spec
             ingredients = T.unpack $ T.unlines $ L.tail bodyLines
         in either (\_ -> return "Error") (return . T.pack) $
              case spec' of
                [a]      -> scaleIngredientsByFactor a ingredients
                [a0, a1] -> scaleIngredientsToQuantity a0 a1 ingredients
                _        -> return "not implemented"

scaleIngredientsByFactor :: String -> String -> Either String String
scaleIngredientsByFactor factorS ingredientsS = do
  factor <- parseNumber factorS
  ingredients <- parseIngredients ingredientsS
  let ingredients' = scaleIngredients factor ingredients
  return $ printIngredientsExtended ingredients'

scaleIngredientsToQuantity :: String -> String -> String -> Either String String
scaleIngredientsToQuantity name quantityS ingredientsS = do
  quantity <- parseQuantity quantityS
  let ingredient = Ingredient { ingredientName = name, ingredientQuantity = quantity }
  ingredients <- parseIngredients ingredientsS
  factor <- computeScalingFactor ingredients ingredient
  let ingredients' = scaleIngredients factor ingredients
  return $ printIngredientsExtended ingredients'

allowedError :: Rational
allowedError = 0.05

printIngredientsExtended :: [Ingredient] -> String
printIngredientsExtended ingredients =
  printIngredientsExt ingredients iExt
  where iExt :: Ingredient -> String
        iExt i =
          let quantity = ingredientQuantity i
              quantities = equivalentQuantities quantity
              quantities' = map ((approximateQuantity allowedError) . roundQuantity) quantities
          in if null quantities'
                then ""
                else " [" ++ (concat $ intersperse ", " (map printQuantity quantities')) ++ "]"
