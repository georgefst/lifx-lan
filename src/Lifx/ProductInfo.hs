module Lifx.ProductInfo where

import Data.Map qualified as Map
import Data.Text (Text)

import Lifx.Product

productInfo :: Map.Map (Int, Text) [Product]
productInfo = Map.fromList
    [
        (
            ( 1
            , "LIFX"
            )
        ,
            [ Product
                { pid = 1
                , name = "LIFX Original 1000"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 3
                , name = "LIFX Color 650"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 10
                , name = "LIFX White 800 (Low Voltage)"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 11
                , name = "LIFX White 800 (High Voltage)"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 15
                , name = "LIFX Color 1000"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 18
                , name = "LIFX White 900 BR30 (Low Voltage)"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 19
                , name = "LIFX White 900 BR30 (High Voltage)"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 20
                , name = "LIFX Color 1000 BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 22
                , name = "LIFX Color 1000"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 27
                , name = "LIFX A19"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 28
                , name = "LIFX BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 29
                , name = "LIFX A19 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 30
                , name = "LIFX BR30 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 31
                , name = "LIFX Z"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 32
                , name = "LIFX Z"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 77
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Nothing
                            , extendedMultizone = True
                            }
                        }
                    , Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 36
                , name = "LIFX Downlight"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 37
                , name = "LIFX Downlight"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 38
                , name = "LIFX Beam"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = True
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 77
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Nothing
                            , extendedMultizone = True
                            }
                        }
                    , Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 39
                , name = "LIFX Downlight White to Warm"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 40
                , name = "LIFX Downlight"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 43
                , name = "LIFX A19"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 44
                , name = "LIFX BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 45
                , name = "LIFX A19 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 46
                , name = "LIFX BR30 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 2
                        , minor = 80
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 49
                , name = "LIFX Mini Color"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 50
                , name = "LIFX Mini White to Warm"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 3
                        , minor = 70
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 51
                , name = "LIFX Mini White"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 52
                , name = "LIFX GU10"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 53
                , name = "LIFX GU10"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 55
                , name = "LIFX Tile"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = True
                    , matrix = True
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 57
                , name = "LIFX Candle"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = True
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 59
                , name = "LIFX Mini Color"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 60
                , name = "LIFX Mini White to Warm"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades =
                    [ Upgrade
                        { major = 3
                        , minor = 70
                        , features = Features
                            { hev = False
                            , color = False
                            , chain = False
                            , matrix = False
                            , relays = False
                            , buttons = False
                            , infrared = False
                            , multizone = False
                            , temperatureRange = Just
                                ( 1500
                                , 9000
                                )
                            , extendedMultizone = False
                            }
                        }
                    ]
                }
            , Product
                { pid = 61
                , name = "LIFX Mini White"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 62
                , name = "LIFX A19"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 63
                , name = "LIFX BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 64
                , name = "LIFX A19 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 65
                , name = "LIFX BR30 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 66
                , name = "LIFX Mini White"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 68
                , name = "LIFX Candle"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = True
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 70
                , name = "LIFX Switch"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = True
                    , buttons = True
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Nothing
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 71
                , name = "LIFX Switch"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = True
                    , buttons = True
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Nothing
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 81
                , name = "LIFX Candle White to Warm"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2200
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 82
                , name = "LIFX Filament Clear"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2100
                        , 2100
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 85
                , name = "LIFX Filament Amber"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2000
                        , 2000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 87
                , name = "LIFX Mini White"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 88
                , name = "LIFX Mini White"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2700
                        , 2700
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 89
                , name = "LIFX Switch"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = True
                    , buttons = True
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Nothing
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 90
                , name = "LIFX Clean"
                , features = Features
                    { hev = True
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 91
                , name = "LIFX Color"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 92
                , name = "LIFX Color"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 93
                , name = "LIFX A19 US"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 94
                , name = "LIFX BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 96
                , name = "LIFX Candle White to Warm"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2200
                        , 6500
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 97
                , name = "LIFX A19"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 98
                , name = "LIFX BR30"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 99
                , name = "LIFX Clean"
                , features = Features
                    { hev = True
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 100
                , name = "LIFX Filament Clear"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2100
                        , 2100
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 101
                , name = "LIFX Filament Amber"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 2000
                        , 2000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 109
                , name = "LIFX A19 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 110
                , name = "LIFX BR30 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 111
                , name = "LIFX A19 Night Vision"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 112
                , name = "LIFX BR30 Night Vision Intl"
                , features = Features
                    { hev = False
                    , color = True
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = True
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 113
                , name = "LIFX Mini WW US"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            , Product
                { pid = 114
                , name = "LIFX Mini WW Intl"
                , features = Features
                    { hev = False
                    , color = False
                    , chain = False
                    , matrix = False
                    , relays = False
                    , buttons = False
                    , infrared = False
                    , multizone = False
                    , temperatureRange = Just
                        ( 1500
                        , 9000
                        )
                    , extendedMultizone = False
                    }
                , upgrades = []
                }
            ]
        )
    ]
