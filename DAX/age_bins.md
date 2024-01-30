Age Bin = 
IF(
    Customers[Age] < 20,
    "Under 20",
    IF(
        Customers[Age] < 30,
        "20-29",
        IF(
            Customers[Age] < 40,
            "30-39",
            IF(
                Customers[Age] < 50,
                "40-49",
                IF(
                    Customers[Age] < 60,
                    "50-59",
                    IF(
                        Customers[Age] < 70,
                        "60-69",
                        IF(
                            Customers[Age] < 80,
                            "70-79",
                            IF(
                                Customers[Age] < 90,
                                "80-89",
                                IF(
                                    Customers[Age] < 100,
                                    "90-99",
                                    IF(
                                        Customers[Age] <= 120,
                                        "100-120",
                                        "Invalid Age Range"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
