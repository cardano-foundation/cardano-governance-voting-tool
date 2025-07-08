module Helper exposing
    ( shortenedHex, prettyAdaLovelace
    , textFieldInline
    , formContainer, boxContainer, cardContainer, cardHeader, cardContent
    , viewButton, viewWalletButton, externalLinkButton
    , applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle
    , sectionTitle, viewError
    , viewStepWithCircle, viewPageHeader
    , PreconfVoter, viewVoterGrid, viewVoterCard, voterCustomCard, votingPowerDisplay, scriptInfoContainer, viewVoterCredDetails, viewVoterDetailsItem, viewCredInfo
    , viewUtxoRefForm, scriptSignerSection, scriptSignerCheckbox, viewIdentifiedVoterCard, viewVoterInfoItem
    )

{-| Helper module for miscellaneous functions that didn't fit elsewhere,
and are potentially useful in multiple places.


# String formatting

@docs shortenedHex, prettyAdaLovelace


# Form elements

@docs textFieldInline


# Containers

@docs formContainer, boxContainer, cardContainer, cardHeader, cardContent


# Buttons

@docs viewButton, viewWalletButton, externalLinkButton


# Wallet Styling

@docs applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle


# UI Structure Components

@docs sectionTitle, viewError


# Page Structure Components

@docs viewStepWithCircle, viewPageHeader


# Voter Identification Components

@docs PreconfVoter, viewVoterGrid, viewVoterCard, voterCustomCard, votingPowerDisplay, scriptInfoContainer, viewVoterCredDetails, viewVoterDetailsItem, viewCredInfo
@docs viewUtxoRefForm, scriptSignerSection, scriptSignerCheckbox, viewIdentifiedVoterCard, viewVoterInfoItem

-}

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onCheck, onClick)
import Natural exposing (Natural)
import Numeral
import RemoteData



-- STRING FORMATTING ###########################################################


{-| Shorten some string, by only keeping the first and last few characters.
-}
shortenedHex : Int -> String -> String
shortenedHex visibleChars str =
    let
        strLength =
            String.length str
    in
    if strLength <= visibleChars * 2 then
        str

    else
        String.slice 0 visibleChars str
            ++ "..."
            ++ String.slice (strLength - visibleChars) strLength str


{-| Display a Lovelace amount as a pretty Ada (₳) amount.

  - `42000 -> "₳0.042"`
  - `69427000000 -> "₳69.43k"`

The number is formatted to automatically use the most adequate unit (k, m, ...).
For amounts below 1₳, the number is formatted with 3 decimals.
For amounts over 1₳, the number is formatted with 2 decimals.

-}
prettyAdaLovelace : Natural -> String
prettyAdaLovelace n =
    Natural.divBy (Natural.fromSafeInt 1000) n
        |> Maybe.withDefault Natural.zero
        -- At this point we have /1000 the amount of lovelace
        -- so for any practical purpose we can make the assumption
        -- that it is within the JS safe integer range
        |> Natural.toInt
        |> (\millis ->
                -- if the amount is above 1 Ada we use .00 precision
                -- otherwise we use .000 precision
                if millis >= 1000 then
                    "₳" ++ Numeral.format "0.00a" (toFloat millis / 1000)

                else
                    "₳" ++ Numeral.format "0.000a" (toFloat millis / 1000)
           )



-- FORM ELEMENTS ###############################################################


textFieldInline : String -> (String -> msg) -> Html msg
textFieldInline value toMsg =
    Html.span [ HA.class "inline-block mr-2" ]
        [ Html.input
            (inputBaseStyle
                ++ [ HA.type_ "text"
                   , HA.value value
                   , Html.Events.onInput toMsg
                   , HA.style "width" "100%"
                   , HA.style "padding" "0.5rem 0"
                   , HA.style "border-radius" "0"
                   , HA.style "outline" "none"
                   , HA.style "box-shadow" "none"
                   ]
            )
            []
        ]


inputBaseStyle : List (Html.Attribute msg)
inputBaseStyle =
    [ HA.style "background-color" "transparent"
    , HA.style "border-top" "none"
    , HA.style "border-left" "none"
    , HA.style "border-right" "none"
    , HA.style "border-bottom" "1px solid #7A7A7A"
    ]


textField : String -> String -> (String -> msg) -> Html msg
textField label value toMsg =
    Html.span [ HA.style "display" "block", HA.style "margin-bottom" "0.5rem" ]
        [ Html.label [] [ text <| label ++ " " ]
        , Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput toMsg
            , HA.style "background-color" "#C6C6C6"
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem 0.75rem"
            ]
            []
        ]



-- CONTAINERS ##################################################################


formContainer : List (Html msg) -> Html msg
formContainer content =
    Html.div [ HA.class "py-4" ] content


boxContainer : List (Html msg) -> Html msg
boxContainer content =
    Html.div
        [ HA.style "background-color" "#ffffff"
        , HA.style "border-radius" "0.5rem"
        , HA.style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
        , HA.style "padding" "1.5rem"
        ]
        content



-- BUTTONS #####################################################################


viewButton : String -> msg -> Html msg
viewButton label msg =
    Html.button
        (onClick msg
            :: HA.style "margin-top" "0.5rem"
            :: HA.style "margin-bottom" "0.5em"
            :: buttonCommonStyle
        )
        [ text label ]


viewWalletButton : String -> msg -> List (Html msg) -> Html msg
viewWalletButton label msg content =
    Html.button
        (onClick msg :: buttonCommonStyle)
        (text label :: content)


externalLinkButton : { url : String, label : String } -> Html msg
externalLinkButton { url, label } =
    Html.a
        [ HA.href url
        , HA.target "_blank"
        , HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "white-space" "nowrap"
        , HA.style "border-radius" "9999px"
        , HA.style "font-size" "0.875rem"
        , HA.style "font-weight" "500"
        , HA.style "transition" "all 0.2s"
        , HA.style "outline" "none"
        , HA.style "background-color" "#272727"
        , HA.style "color" "#f7fafc"
        , HA.style "padding" "0.75rem 1.5rem"
        ]
        [ text <| label ++ " ↗" ]


buttonCommonStyle : List (Html.Attribute msg)
buttonCommonStyle =
    [ HA.style "display" "inline-flex"
    , HA.style "align-items" "center"
    , HA.style "justify-content" "center"
    , HA.style "white-space" "nowrap"
    , HA.style "border-radius" "9999px"
    , HA.style "font-size" "0.875rem"
    , HA.style "font-weight" "500"
    , HA.style "transition" "all 0.2s"
    , HA.style "outline" "none"
    , HA.style "ring-offset" "background"
    , HA.style "focus-visible:ring" "2px"
    , HA.style "focus-visible:ring-color" "ring"
    , HA.style "focus-visible:ring-offset" "2px"
    , HA.style "background-color" "#272727"
    , HA.style "color" "#f7fafc"
    , HA.style "hover:bg-color" "#f9fafb"
    , HA.style "hover:text-color" "#1a202c"
    , HA.style "height" "3rem"
    , HA.style "padding-left" "1.5rem"
    , HA.style "padding-right" "1.5rem"
    ]



-- WALLET STYLING ##############################################################


applyDropdownContainerStyle : List (Html.Attribute msg)
applyDropdownContainerStyle =
    HA.style "top" "100%"
        :: HA.style "right" "0"
        :: HA.style "width" "220px"
        :: baseDropdownContainerStyle


applyMobileDropdownContainerStyle : List (Html.Attribute msg)
applyMobileDropdownContainerStyle =
    HA.style "width" "100%"
        :: baseDropdownContainerStyle


baseDropdownContainerStyle : List (Html.Attribute msg)
baseDropdownContainerStyle =
    [ HA.style "position" "absolute"
    , HA.style "margin-top" "0.5rem"
    , HA.style "background-color" "#f8f9fa"
    , HA.style "border" "1px solid #e2e8f0"
    , HA.style "border-radius" "0.5rem"
    , HA.style "box-shadow" "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"
    , HA.style "z-index" "50"
    , HA.style "padding" "0.5rem 0"
    , HA.style "max-height" "300px"
    , HA.style "overflow-y" "auto"
    ]


applyDropdownItemStyle : msg -> List (Html.Attribute msg)
applyDropdownItemStyle onClickMsg =
    [ HA.class "px-4 py-2 hover:bg-gray-100 cursor-pointer flex items-center"
    , onClick onClickMsg
    , HA.style "padding-left" "1rem"
    , HA.style "padding-right" "1rem"
    , HA.style "padding-top" "0.5rem"
    , HA.style "padding-bottom" "0.5rem"
    , HA.style "cursor" "pointer"
    , HA.style "display" "flex"
    , HA.style "align-items" "center"
    ]


applyWalletIconContainerStyle : List (Html.Attribute msg)
applyWalletIconContainerStyle =
    [ HA.style "width" "20px"
    , HA.style "height" "20px"
    , HA.style "margin-right" "0.5rem"
    , HA.style "display" "flex"
    , HA.style "align-items" "center"
    , HA.style "justify-content" "center"
    ]


applyWalletIconStyle : List (Html.Attribute msg)
applyWalletIconStyle =
    [ HA.style "max-height" "20px"
    , HA.style "max-width" "20px"
    , HA.style "object-fit" "contain"
    ]



-- SECTION STYLING #############################################################


{-| Standard section title with consistent styling
-}
sectionTitle : String -> Html msg
sectionTitle title =
    Html.h2
        [ HA.style "font-weight" "500"
        , HA.style "font-size" "1.875rem"
        , HA.style "color" "#1A202C"
        , HA.style "margin-top" "0.7rem"
        , HA.style "margin-bottom" "1rem"
        ]
        [ text title ]



-- CARD STYLING ################################################################


{-| Container for card-style UI elements with standard styling
-}
cardContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
cardContainer attributes content =
    div
        ([ HA.style "border" "1px solid #E2E8F0"
         , HA.style "border-radius" "0.75rem"
         , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
         , HA.style "background-color" "#FFFFFF"
         , HA.style "overflow" "hidden"
         , HA.style "margin-bottom" "1.5rem"
         ]
            ++ attributes
        )
        content


{-| Standard header section for cards with title styling
-}
cardHeader : List (Html.Attribute msg) -> String -> String -> List (Html msg) -> Html msg
cardHeader attributes title subtitle extraContent =
    div
        ([ HA.style "background-color" "#F7FAFC"
         , HA.style "padding" "1rem 1.25rem"
         , HA.style "border-bottom" "1px solid #EDF2F7"
         ]
            ++ attributes
        )
        (div []
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1.125rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text title ]
            , Html.p [ HA.style "font-size" "0.875rem" ] [ text subtitle ]
            ]
            :: extraContent
        )


{-| Standard content section for cards with consistent padding
-}
cardContent : List (Html.Attribute msg) -> List (Html msg) -> Html msg
cardContent attributes content =
    div
        (HA.style "padding" "1.25rem" :: attributes)
        content



-- UI COMPONENTS ##############################################################


{-| Generic error display component
-}
viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            div
                [ HA.style "background-color" "#FEF2F2"
                , HA.style "border" "1px solid #FEE2E2"
                , HA.style "border-radius" "0.5rem"
                , HA.style "padding" "1rem"
                , HA.style "margin-top" "1rem"
                ]
                [ Html.div
                    [ HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.span
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "bold"
                        , HA.style "margin-right" "0.5rem"
                        , HA.style "font-size" "1.25rem"
                        ]
                        [ text "!" ]
                    , Html.p
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "600"
                        , HA.style "margin" "0"
                        ]
                        [ text "Error" ]
                    ]
                , Html.pre
                    [ HA.style "background-color" "#FFFFFF"
                    , HA.style "border" "1px solid #FEE2E2"
                    , HA.style "border-radius" "0.25rem"
                    , HA.style "padding" "0.75rem"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "white-space" "pre-wrap"
                    , HA.style "overflow-x" "auto"
                    , HA.style "font-family" "monospace"
                    , HA.style "color" "#991B1B"
                    , HA.style "margin" "0"
                    ]
                    [ text err ]
                ]



-- Stepper with circle and header #########################################################


{-| Renders a step with a numbered circle on the left side
-}
viewStepWithCircle : Int -> String -> Html msg -> Html msg
viewStepWithCircle stepNumber stepId content =
    div
        [ HA.id stepId
        , HA.style "position" "relative"
        , HA.style "padding-left" "5rem"
        , HA.style "padding-top" "2rem"
        , HA.style "padding-bottom" "2rem"
        ]
        [ -- Circle with step number
          div
            [ HA.style "position" "absolute"
            , HA.style "left" "-0.10rem"
            , HA.style "top" "2.9rem"
            , HA.style "width" "2.5rem"
            , HA.style "height" "2.5rem"
            , HA.style "border-radius" "50%"
            , HA.style "background-color" "#272727"
            , HA.style "color" "white"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "font-weight" "bold"
            , HA.style "font-size" "1.125rem"
            , HA.style "z-index" "3"
            , HA.style "box-shadow" "0 0 0 4px white"
            ]
            [ text (String.fromInt stepNumber) ]
        , content
        ]


{-| Renders the page header with title and description
-}
viewPageHeader : Html msg
viewPageHeader =
    div
        [ HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        , HA.style "padding-top" "6rem"
        , HA.style "padding-bottom" "6rem"
        , HA.style "margin-bottom" "2rem"
        ]
        [ div
            [ HA.style "position" "relative"
            , HA.style "z-index" "10"
            , HA.style "max-width" "840px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1.5rem"
            ]
            [ Html.h1
                [ HA.style "font-size" "3.5rem"
                , HA.style "font-weight" "600"
                , HA.style "line-height" "1.1"
                , HA.style "margin-bottom" "1.5rem"
                ]
                [ text "Vote Preparation" ]
            , Html.p
                [ HA.style "font-size" "1.25rem"
                , HA.style "line-height" "1.6"
                , HA.style "max-width" "640px"
                , HA.style "margin-bottom" "2rem"
                ]
                [ text "This page helps you prepare and submit votes for governance proposals. You can identify yourself as a voter, select a proposal, create a rationale for your vote, and build the transaction."
                ]
            ]
        , viewHeaderBackground
        ]


{-| Renders the gradient background for the header
-}
viewHeaderBackground : Html msg
viewHeaderBackground =
    div
        [ HA.style "position" "absolute"
        , HA.style "z-index" "1"
        , HA.style "top" "-13rem"
        , HA.style "right" "0"
        , HA.style "left" "0"
        , HA.style "overflow" "hidden"
        , HA.style "transform" "translateZ(0)"
        , HA.style "filter" "blur(64px)"
        ]
        [ div
            [ HA.style "position" "relative"
            , HA.style "width" "100%"
            , HA.style "padding-bottom" "58.7%"
            , HA.style "background" "linear-gradient(90deg, #00E0FF, #0084FF)"
            , HA.style "opacity" "0.8"
            , HA.style "clip-path" "polygon(19% 5%, 36% 8%, 55% 15%, 76% 5%, 100% 16%, 100% 100%, 0 100%, 0 14%)"
            ]
            []
        ]



-- VOTER IDENTIFICATION STEP STYLING #########################################################


{-| Grid layout for voter role cards
-}
viewVoterGrid : List (Html msg) -> Html msg
viewVoterGrid cards =
    if List.isEmpty cards then
        text ""

    else
        viewGrid 220 cards


viewGrid : Int -> List (Html msg) -> Html msg
viewGrid minmaxWidth elems =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" ("repeat(auto-fill, minmax(" ++ String.fromInt minmaxWidth ++ "px, 1fr))")
        , HA.style "gap" "1.5rem"
        , HA.style "margin-bottom" "1.5rem"
        ]
        elems


type alias PreconfVoter =
    { voterType : String, description : String, govId : String }


{-| Card for displaying a voter role option
-}
viewVoterCard : (String -> msg) -> String -> PreconfVoter -> Html msg
viewVoterCard selectMsg currentSelection { voterType, description, govId } =
    let
        isSelected =
            currentSelection == govId
    in
    div
        [ HA.style "border"
            (if isSelected then
                "2px solid #272727"

             else
                "1px solid #E2E8F0"
            )
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        , HA.style "transform-origin" "center"
        , HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        , HA.style "cursor" "pointer"
        , onClick (selectMsg govId)
        ]
        [ div
            [ HA.style "background-color"
                (if isSelected then
                    "#F1F5F9"

                 else
                    "#F7FAFC"
                )
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text voterType ]
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "margin-bottom" "1rem"
                ]
                [ text description ]
            , div
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#718096"
                , HA.style "margin-top" "auto"
                ]
                [ div
                    [ HA.style "overflow" "hidden"
                    , HA.style "text-overflow" "ellipsis"
                    , HA.style "white-space" "nowrap"
                    , HA.style "font-family" "monospace"
                    ]
                    [ text (String.left 8 govId ++ "..." ++ String.right 8 govId) ]
                ]
            ]
        , if isSelected then
            div
                [ HA.style "position" "absolute"
                , HA.style "top" "0.5rem"
                , HA.style "right" "0.5rem"
                , HA.style "width" "1.5rem"
                , HA.style "height" "1.5rem"
                , HA.style "border-radius" "9999px"
                , HA.style "background-color" "#272727"
                , HA.style "color" "white"
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                ]
                [ text "✓" ]

          else
            text ""
        ]


{-| Card for custom voter ID input
-}
voterCustomCard : { currentValue : String, onInputMsg : String -> msg } -> Html msg
voterCustomCard { currentValue, onInputMsg } =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        ]
        [ div
            [ HA.style "background-color" "#F7FAFC"
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text "Custom" ]
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "margin-bottom" "1rem"
                ]
                [ text "Enter your own governance ID" ]
            , Html.div
                [ HA.style "position" "relative"
                , HA.style "width" "100%"
                ]
                [ Html.input
                    [ HA.type_ "text"
                    , HA.value currentValue
                    , HA.placeholder "Paste drep/pool/cc_hot ID"
                    , Html.Events.onInput onInputMsg
                    , HA.style "width" "100%"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border" "1px solid #CBD5E0"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "font-family" "monospace"
                    , HA.style "font-size" "0.75rem"
                    , HA.style "box-sizing" "border-box"
                    ]
                    []
                ]
            ]
        ]


{-| Display voting power information for a voter
-}
votingPowerDisplay : (a -> Int) -> RemoteData.WebData a -> Html msg
votingPowerDisplay accessor webData =
    case webData of
        RemoteData.NotAsked ->
            text "not querried"

        RemoteData.Loading ->
            text "loading ..."

        RemoteData.Failure _ ->
            text "Is this the correct network? If yes, then most likely, this voter is inactive, not registered yet, or was just registered this epoch."

        RemoteData.Success success ->
            text <| prettyAdaLovelace <| Natural.fromSafeInt <| accessor success


{-| Container for script information
-}
scriptInfoContainer : List (Html msg) -> Html msg
scriptInfoContainer content =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "padding" "1.25rem"
        , HA.style "margin-top" "1.5rem"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "0.75rem"
            ]
            content
        ]


{-| Display details about a credential (key or script)
-}
viewVoterCredDetails : String -> String -> Html msg
viewVoterCredDetails label hashValue =
    div [ HA.style "display" "flex", HA.style "align-items" "center" ]
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-right" "0.5rem"
            , HA.style "min-width" "12rem"
            ]
            [ text label ]
        , Html.span
            [ HA.style "font-family" "monospace"
            , HA.style "background-color" "#EDF2F7"
            , HA.style "padding" "0.25rem 0.5rem"
            , HA.style "border-radius" "0.25rem"
            ]
            [ text hashValue ]
        ]


{-| Display details for a voter item
-}
viewVoterDetailsItem : String -> Html msg -> Html msg
viewVoterDetailsItem label content =
    div [ HA.style "display" "flex", HA.style "align-items" "center" ]
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-right" "0.5rem"
            , HA.style "min-width" "12rem"
            ]
            [ text label ]
        , content
        ]


{-| Container for constitutional committee information
-}
viewCredInfo : List (Html msg) -> Html msg
viewCredInfo content =
    div [] content


{-| Form for a UTxO reference field
-}
viewUtxoRefForm : String -> (String -> msg) -> Html msg
viewUtxoRefForm utxoRef onInputMsg =
    Html.p [] [ textField "Reference UTxO" utxoRef onInputMsg ]


{-| Section for script signers with fee info
-}
scriptSignerSection : Int -> Int -> List (Html msg) -> Html msg
scriptSignerSection additionalBytesPerSig feePerByte content =
    let
        additionalSignerCost =
            Natural.fromSafeInt <| feePerByte * additionalBytesPerSig
    in
    div []
        [ Html.p []
            [ text <| "Expected signers: (each adds " ++ prettyAdaLovelace additionalSignerCost ++ " to the Tx fees)" ]
        , div [] content
        ]


{-| Checkbox for script signers
-}
scriptSignerCheckbox : String -> Bool -> (Bool -> msg) -> Html msg
scriptSignerCheckbox keyHex isChecked onCheckMsg =
    Html.p []
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.id keyHex
            , HA.name keyHex
            , HA.checked isChecked
            , onCheck onCheckMsg
            ]
            []
        , Html.label [ HA.for keyHex ] [ text <| " key hash: " ++ keyHex ]
        ]


{-| Container for voter identification card
-}
viewVoterIdentificationCard : String -> List (Html msg) -> Html msg
viewVoterIdentificationCard title content =
    cardContainer []
        [ cardHeader [] title "" []
        , cardContent [] content
        ]


{-| Display identified voter card with information
-}
viewIdentifiedVoterCard : String -> List (Html msg) -> Html msg -> Html msg
viewIdentifiedVoterCard title content changeButton =
    div []
        [ sectionTitle "Voter Information"
        , viewVoterIdentificationCard title content
        , Html.p
            [ HA.style "margin-top" "1rem" ]
            [ changeButton ]
        ]


{-| Display voter info item
-}
viewVoterInfoItem : String -> String -> Html msg
viewVoterInfoItem label value =
    Html.p [] [ text <| label ++ ": " ++ value ]
