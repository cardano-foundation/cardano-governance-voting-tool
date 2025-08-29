module Header exposing (ViewContext, view)

import Cardano.Address exposing (NetworkId(..))
import Helper
import Html exposing (Html, button, div, img, li, nav, span, text, ul)
import Html.Attributes exposing (alt, class, id, src, style)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SA
import WalletConnector


type alias ViewContext msg =
    { mobileMenuIsOpen : Bool
    , toggleMobileMenu : msg
    , networkDropdownIsOpen : Bool
    , toggleNetworkDropdown : msg
    , walletConnector : WalletConnector.State
    , walletConnectorMsgs : WalletConnector.Msgs msg
    , logoLink : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , navigationItems :
        List
            { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
            , label : String
            , isActive : Bool
            }
    , networkId : NetworkId
    , onNetworkChange : NetworkId -> msg
    , cartCount : Int
    , cartLink : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    }


view : ViewContext msg -> Html msg
view { mobileMenuIsOpen, toggleMobileMenu, networkDropdownIsOpen, toggleNetworkDropdown, walletConnector, walletConnectorMsgs, logoLink, navigationItems, networkId, onNetworkChange, cartCount, cartLink } =
    nav
        [ class "w-full"
        , style "position" "sticky"
        , style "top" "0"
        , style "z-index" "100"
        , style "border-bottom" "1px solid rgba(226,232,240,0.1)"
        , style "background-color" "rgba(255,255,255,0.05)"
        , style "backdrop-filter" "saturate(180%) blur(16px)"
        , style "-webkit-backdrop-filter" "saturate(180%) blur(16px)"
        ]
        [ div
            [ class "mx-auto py-4 md:py-6 overflow-visible"
            , style "max-width" "1100px"
            , style "padding-left" "1rem"
            , style "padding-right" "1rem"
            ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo section
                [ div [ style "flex-shrink" "0" ]
                    [ logoLink
                        [ style "display" "flex"
                        , style "align-items" "center"
                        ]
                        [ img
                            [ src "/logo/foundation-logo.svg"
                            , alt "Logo"
                            , style "width" "auto"
                            , style "margin-right" "8px"
                            , style "margin-left" "8px"
                            , style "max-height" "40px"
                            ]
                            []
                        , span
                            [ style "font-weight" "700"
                            , style "font-size" "1.125rem"
                            ]
                            [ text "" ]
                        ]
                    ]

                -- Desktop menu
                , div [ class "hidden md:flex", style "gap" "1rem" ]
                    (List.map viewDesktopMenuItem navigationItems)

                -- Wallet, network selector and cart button
                , div [ class "hidden md:flex items-center", style "gap" "0.5rem" ]
                    [ WalletConnector.view walletConnectorMsgs walletConnector
                    , viewNetworkSelector
                        networkId
                        networkDropdownIsOpen
                        toggleNetworkDropdown
                        onNetworkChange
                    , cartLink [] [ viewCartButton "36px" "ml-2" cartCount ]
                    ]

                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none"
                        , Html.Attributes.attribute "aria-label"
                            (if mobileMenuIsOpen then
                                "Close menu"

                             else
                                "Open menu"
                            )
                        , onClick toggleMobileMenu
                        ]
                        [ if mobileMenuIsOpen then
                            Svg.svg
                                [ SA.width "24"
                                , SA.height "24"
                                , SA.viewBox "0 0 24 24"
                                , SA.fill "none"
                                , SA.stroke "#1F2937"
                                , SA.strokeWidth "2.5"
                                , SA.strokeLinecap "round"
                                , SA.strokeLinejoin "round"
                                , SA.class "mr-4"
                                ]
                                [ Svg.path [ SA.d "M6 6l12 12" ] []
                                , Svg.path [ SA.d "M18 6L6 18" ] []
                                ]

                          else
                            Svg.svg
                                [ SA.width "24"
                                , SA.height "24"
                                , SA.viewBox "0 0 24 24"
                                , SA.fill "none"
                                , SA.stroke "#1F2937"
                                , SA.strokeWidth "2.5"
                                , SA.strokeLinecap "round"
                                , SA.strokeLinejoin "round"
                                , SA.class "mr-4"
                                ]
                                [ Svg.path [ SA.d "M4 7h16" ] []
                                , Svg.path [ SA.d "M4 12h16" ] []
                                , Svg.path [ SA.d "M4 17h16" ] []
                                ]
                        ]
                    ]
                ]

            -- Mobile menu
            , div
                [ class
                    ("md:hidden transition-all duration-300 ease-in-out "
                        ++ (if mobileMenuIsOpen then
                                "max-h-[500px] overflow-visible"

                            else
                                "max-h-0 overflow-hidden"
                           )
                    )
                ]
                [ div [ class "py-2" ]
                    [ div
                        [ style "height" "1px"
                        , style "background-color" "#94A3B8"
                        , style "width" "100vw"
                        , style "margin-left" "calc(50% - 50vw)"
                        , style "margin-right" "calc(50% - 50vw)"
                        , style "margin-top" "0.5rem"
                        , style "margin-bottom" "0.75rem"
                        ]
                        []
                    , ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map viewMobileMenuItem navigationItems)
                    , div [ class "px-4" ]
                        [ div
                            [ style "height" "1px"
                            , style "background-color" "#7B8598"
                            , style "margin-top" "0.5rem"
                            , style "margin-bottom" "0.75rem"
                            ]
                            []
                        ]
                    , div [ class "mt-4 px-4 z-20 relative" ]
                        [ div []
                            [ WalletConnector.viewMobile walletConnectorMsgs walletConnector
                            , div
                                [ style "display" "flex"
                                , style "align-items" "center"
                                , style "justify-content" "flex-start"
                                , style "gap" "0.5rem"
                                ]
                                [ viewMobileNetworkSelector networkId onNetworkChange
                                , cartLink [ class "ml-2 mt-2" ] [ viewCartButton "44px" "" cartCount ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewCartButton : String -> String -> Int -> Html msg
viewCartButton buttonSize additionalClasses cartCount =
    button
        [ class additionalClasses
        , style "position" "relative"
        , style "display" "inline-flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "width" buttonSize
        , style "height" buttonSize
        , style "border-radius" "9999px"
        , style "background-color" "#272727"
        , style "color" "#f7fafc"
        , style "border" "none"
        , style "box-shadow" "0 2px 6px rgba(0,0,0,0.06)"
        , id "cart-button"
        , Html.Attributes.attribute "aria-label" "Open cart"
        , Html.Attributes.title "Cart"
        ]
        (Svg.svg
            [ SA.width "22"
            , SA.height "22"
            , SA.viewBox "0 0 24 24"
            , SA.fill "none"
            , SA.stroke "#FFFFFF"
            , SA.strokeWidth "2.5"
            , SA.strokeLinecap "round"
            , SA.strokeLinejoin "round"
            ]
            [ Svg.circle [ SA.cx "9", SA.cy "19", SA.r "1.5", SA.fill "none", SA.stroke "#FFFFFF" ] []
            , Svg.circle [ SA.cx "17", SA.cy "19", SA.r "1.5", SA.fill "none", SA.stroke "#FFFFFF" ] []
            , Svg.path [ SA.d "M3 4h2l2 9c.2.9 1 1.5 1.9 1.5H17c.9 0 1.7-.6 1.9-1.5L21 7H6", SA.fill "none", SA.stroke "#FFFFFF" ] []
            ]
            :: viewCartBadge cartCount
        )


{-| Small helper to render the cart badge consistently in desktop and mobile.
-}
viewCartBadge : Int -> List (Html msg)
viewCartBadge cartCount =
    if cartCount > 0 then
        [ span
            [ style "position" "absolute"
            , style "top" "-6px"
            , style "right" "-6px"
            , style "min-width" "18px"
            , style "height" "18px"
            , style "padding" "0 4px"
            , style "border-radius" "9999px"
            , style "background-color" "#10B981"
            , style "color" "white"
            , style "font-size" "11px"
            , style "font-weight" "700"
            , style "line-height" "18px"
            , style "display" "inline-flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "box-shadow" "0 1px 2px rgba(0,0,0,0.12)"
            ]
            [ text (String.fromInt cartCount) ]
        ]

    else
        []


viewDesktopMenuItem :
    { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , label : String
    , isActive : Bool
    }
    -> Html msg
viewDesktopMenuItem item =
    item.link
        [ class
            ("px-4 py-2 font-medium transition-all duration-200 "
                ++ (if item.isActive then
                        "text-gray-900 border-b-2 border-gray-800"

                    else
                        "text-gray-800 hover:text-gray-900 hover:border-b-2 hover:border-gray-800"
                   )
            )
        ]
        [ text item.label ]


viewMobileMenuItem :
    { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , label : String
    , isActive : Bool
    }
    -> Html msg
viewMobileMenuItem item =
    li []
        [ item.link
            [ class
                ("block px-4 py-2 rounded-md transition-colors duration-200 "
                    ++ (if item.isActive then
                            "text-gray-900 bg-gray-100"

                        else
                            "text-gray-800 hover:bg-gray-100"
                       )
                )
            ]
            [ text item.label ]
        ]


viewNetworkSelector : NetworkId -> Bool -> msg -> (NetworkId -> msg) -> Html msg
viewNetworkSelector currentNetwork dropdownOpen toggleDropdown onNetworkChange =
    let
        isMainnet =
            currentNetwork == Mainnet

        networkLabel =
            if isMainnet then
                "Mainnet"

            else
                "Preview"

        otherNetwork =
            if isMainnet then
                Testnet

            else
                Mainnet

        otherNetworkLabel =
            if isMainnet then
                "Preview"

            else
                "Mainnet"

        networkColor =
            if isMainnet then
                "#10b981"

            else
                "#3b82f6"

        -- Green for Mainnet, Blue for Preview
    in
    div [ style "position" "relative", style "margin-left" "0.5rem" ]
        [ Helper.viewWalletButton networkLabel
            toggleDropdown
            [ -- Network indicator dot
              div
                [ style "width" "0.75rem"
                , style "height" "0.75rem"
                , style "border-radius" "9999px"
                , style "background-color" networkColor
                , style "margin-left" "0.2rem"
                ]
                []
            , span
                [ style "transition-transform" "0.2s"
                , style "margin-left" "4px"
                , style "transform"
                    (if dropdownOpen then
                        "rotate(180deg)"

                     else
                        "rotate(0)"
                    )
                ]
                [ text "▼" ]
            ]
        , if dropdownOpen then
            div Helper.applyDropdownContainerStyle
                [ ul []
                    [ li (Helper.applyDropdownItemStyle (onNetworkChange otherNetwork))
                        [ -- Network indicator dot for other network
                          div
                            [ style "width" "0.75rem"
                            , style "height" "0.75rem"
                            , style "border-radius" "9999px"
                            , style "background-color"
                                (if not isMainnet then
                                    "#10b981"

                                 else
                                    "#3b82f6"
                                )
                            , style "margin-right" "0.5rem"
                            ]
                            []
                        , text otherNetworkLabel
                        ]
                    ]
                ]

          else
            text ""
        ]


viewMobileNetworkSelector : NetworkId -> (NetworkId -> msg) -> Html msg
viewMobileNetworkSelector currentNetwork onNetworkChange =
    let
        ( networkAsText, networkColor, otherNetwork ) =
            case currentNetwork of
                Mainnet ->
                    ( "Mainnet", "#10b981", Testnet )

                Testnet ->
                    ( "Preview", "#3b82f6", Mainnet )
    in
    div
        [ style "display" "inline-flex"
        , style "align-items" "center"
        , style "padding" "0.75rem 2.25rem"
        , style "margin-top" "0.5rem"
        , style "border-radius" "9999px"
        , style "background-color" "#272727"
        , style "color" "white"
        , style "cursor" "pointer"
        , style "font-size" "0.875rem"
        , style "font-weight" "500"
        , style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
        , style "transition" "background-color 0.2s"
        , onClick (onNetworkChange otherNetwork)
        ]
        [ div
            [ style "width" "0.75rem"
            , style "height" "0.75rem"
            , style "border-radius" "9999px"
            , style "background-color" networkColor
            , style "margin-right" "0.5rem"
            ]
            []
        , text networkAsText
        , div
            [ style "margin-left" "0.5rem"
            , style "font-size" "0.75rem"
            ]
            [ text "↺" ]
        ]
