-- | This is auto-generated - see the "product-gen" script.
module Lifx.Internal.ProductInfo where

import Lifx.Internal.Product

productInfo :: [VendorInfo]
productInfo =
    [ VendorInfo
        { vid = 1
        , name = "LIFX"
        , defaults = Features
            { hev = False
            , color = False
            , chain = False
            , matrix = False
            , relays = False
            , buttons = False
            , infrared = False
            , multizone = False
            , temperatureRange = Nothing
            , extendedMultizone = False
            }
        , products =
            [ ProductInfo
                { pid = 1
                , name = "LIFX Original 1000"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 3
                , name = "LIFX Color 650"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 10
                , name = "LIFX White 800 (Low Voltage)"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 11
                , name = "LIFX White 800 (High Voltage)"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 15
                , name = "LIFX Color 1000"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 18
                , name = "LIFX White 900 BR30 (Low Voltage)"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 19
                , name = "LIFX White 900 BR30 (High Voltage)"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 20
                , name = "LIFX Color 1000 BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 22
                , name = "LIFX Color 1000"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 27
                , name = "LIFX A19"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 28
                , name = "LIFX BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 29
                , name = "LIFX A19 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 30
                , name = "LIFX BR30 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 31
                , name = "LIFX Z"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 32
                , name = "LIFX Z"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 77
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Nothing
                            , extendedMultizone = Just True
                            }
                        }
                    , Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 36
                , name = "LIFX Downlight"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 37
                , name = "LIFX Downlight"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 38
                , name = "LIFX Beam"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 77
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Nothing
                            , extendedMultizone = Just True
                            }
                        }
                    , Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 39
                , name = "LIFX Downlight White to Warm"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 40
                , name = "LIFX Downlight"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 43
                , name = "LIFX A19"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 44
                , name = "LIFX BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 45
                , name = "LIFX A19 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 46
                , name = "LIFX BR30 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 49
                , name = "LIFX Mini Color"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 50
                , name = "LIFX Mini White to Warm"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 3
                        , minor = 70
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 51
                , name = "LIFX Mini White"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 52
                , name = "LIFX GU10"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 53
                , name = "LIFX GU10"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 55
                , name = "LIFX Tile"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just True
                    , matrix = Just True
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 57
                , name = "LIFX Candle"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just True
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 59
                , name = "LIFX Mini Color"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 60
                , name = "LIFX Mini White to Warm"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades =
                    [ Upgrade
                        { major = 3
                        , minor = 70
                        , features = PartialFeatures
                            { hev = Nothing
                            , color = Nothing
                            , chain = Nothing
                            , matrix = Nothing
                            , relays = Nothing
                            , buttons = Nothing
                            , infrared = Nothing
                            , multizone = Nothing
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = Nothing
                            }
                        }
                    ]
                }
            , ProductInfo
                { pid = 61
                , name = "LIFX Mini White"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 62
                , name = "LIFX A19"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 63
                , name = "LIFX BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 64
                , name = "LIFX A19 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 65
                , name = "LIFX BR30 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 66
                , name = "LIFX Mini White"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 68
                , name = "LIFX Candle"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just True
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 70
                , name = "LIFX Switch"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Just True
                    , buttons = Just True
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Nothing
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 71
                , name = "LIFX Switch"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Just True
                    , buttons = Just True
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Nothing
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 81
                , name = "LIFX Candle White to Warm"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2200
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 82
                , name = "LIFX Filament Clear"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2100
                        , 2100
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 85
                , name = "LIFX Filament Amber"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2000
                        , 2000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 87
                , name = "LIFX Mini White"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 88
                , name = "LIFX Mini White"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 89
                , name = "LIFX Switch"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Just True
                    , buttons = Just True
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Nothing
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 90
                , name = "LIFX Clean"
                , features = PartialFeatures
                    { hev = Just True
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 91
                , name = "LIFX Color"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 92
                , name = "LIFX Color"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 93
                , name = "LIFX A19 US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 94
                , name = "LIFX BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 96
                , name = "LIFX Candle White to Warm"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2200
                        , 6500
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 97
                , name = "LIFX A19"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 98
                , name = "LIFX BR30"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 99
                , name = "LIFX Clean"
                , features = PartialFeatures
                    { hev = Just True
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 100
                , name = "LIFX Filament Clear"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2100
                        , 2100
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 101
                , name = "LIFX Filament Amber"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2000
                        , 2000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 109
                , name = "LIFX A19 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 110
                , name = "LIFX BR30 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 111
                , name = "LIFX A19 Night Vision"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 112
                , name = "LIFX BR30 Night Vision Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just True
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 113
                , name = "LIFX Mini WW US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 114
                , name = "LIFX Mini WW Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 115
                , name = "LIFX Switch"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Just True
                    , buttons = Just True
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Nothing
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 116
                , name = "LIFX Switch"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Just True
                    , buttons = Just True
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Nothing
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 117
                , name = "LIFX Z"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 118
                , name = "LIFX Z"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 119
                , name = "LIFX Beam"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 120
                , name = "LIFX Beam"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just True
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 123
                , name = "LIFX Color US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 124
                , name = "LIFX Color Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 125
                , name = "LIFX White to Warm US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 126
                , name = "LIFX White to Warm Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 127
                , name = "LIFX White US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 128
                , name = "LIFX White Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 129
                , name = "LIFX Color US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 130
                , name = "LIFX Color Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 131
                , name = "LIFX White To Warm US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 132
                , name = "LIFX White To Warm Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 133
                , name = "LIFX White US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 134
                , name = "LIFX White Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just False
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 135
                , name = "LIFX GU10 Color US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 136
                , name = "LIFX GU10 Color Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just False
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 137
                , name = "LIFX Candle Color US"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just True
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            , ProductInfo
                { pid = 138
                , name = "LIFX Candle Color Intl"
                , features = PartialFeatures
                    { hev = Nothing
                    , color = Just True
                    , chain = Just False
                    , matrix = Just True
                    , relays = Nothing
                    , buttons = Nothing
                    , infrared = Just False
                    , multizone = Just False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = Nothing
                    }
                , upgrades = []
                }
            ]
        }
    ]
