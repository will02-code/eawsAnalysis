# get_bird_data stops on invalid program

    Code
      get_bird_data(dataset = data_clean, programs = "not_a_program", wetlands = NULL,
        valleys = NULL, basinDiv = NULL, grouping_cols = NULL, metric = "abundance")
    Condition
      Error in `get_bird_data()`:
      ! Invalid program. Choose from 'eastern australian survey', 'eaws', 'mdb combined', or 'mdbws'.

# get_bird_data stops on invalid metric

    Code
      get_bird_data(dataset = data_clean, programs = "eaws", wetlands = NULL,
        valleys = NULL, basinDiv = NULL, grouping_cols = NULL, metric = "not_a_metric")
    Condition
      Error in `dplyr::mutate()`:
      ! Invalid metric. Choose from 'abundance', 'richness', 'nests', 'broods', 'nest_richness', 'simpson', or 'pct_filled'.

# get_bird_data stops on invalid wetland

    Code
      get_bird_data(dataset = data_clean, programs = "eaws", wetlands = "not_a_wetland",
        valleys = NULL, basinDiv = NULL, grouping_cols = NULL, metric = "abundance")
    Condition
      Error in `get_bird_data()`:
      ! Invalid wetlands. Value should be one of: NA, corop wetlands complex, lake mokoan, pyap lagoon, noora evaporation basin, murray river and euston lakes, lowbidgee floodplain, fivebough, menindee lakes, talywalka system, burrendong dam, paroo overflow lakes complex, cuttaburra channels, upper darling river, macquarie marshes, currawinya lakes, coolmunda dam, kiewa river, barmah-millewa, hattah lakes, lindsay-walpolla-chowilla, lower lakes, coorong and murray mouth, mulwala, gunbower-koondrook-perricoota, kerang wetlands, yantabulla, narran lakes, darling anabranch, great cumbung swamp, booligal wetlands, lake brewster, lake cowal and nerang cowal, gwydir wetlands, banrock station wetland complex, lake albacutya, lake hindmarsh, lake buloke

