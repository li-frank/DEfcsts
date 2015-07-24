INSERT INTO p_csi_tbs_t.fl_DEvert_actuals

SELECT

			GEM.CK_DATE ckDate,
            CAL.RTL_WEEK_BEG_DT,
            CAL.RETAIL_WEEK, 
            CAL.RETAIL_YEAR,
            
            CASE GEM.SLR_CNTRY_ID
                        WHEN 77 THEN 'DE'
                        ELSE 'Other'
            END AS SLR_CNTRY,
            
            CASE
                        WHEN HIST.USEGM_ID = 206 THEN 'B2C'
                        ELSE 'C2C'
            END AS BIZ_FLAG,

            CASE CAT.BSNS_VRTCL_NAME
                        WHEN 'Electronics' THEN 'Elec'
                        WHEN 'Home & Garden' THEN 'H&G'
                        WHEN 'Parts & Accessories' THEN 'P&A'
                        WHEN 'Business & Industrial' THEN 'B&I'
                        ELSE CAT.BSNS_VRTCL_NAME
            END AS VERTICAL,
            
            SUM(GEM.SOLD_ITEMS) AS SOLD_ITEMS,
            SUM(GEM.GMV_LC) AS GMV_LC,
            SUM(GEM.GMV_PLAN) AS GMV_PLAN
            
FROM
            (SELECT 
                        GEM.LSTG_ID,
                        GEM.LSTG_END_DT,
                        GEM.CK_DATE,
                        GEM.LSTG_SITE_ID,
                        GEM.SLR_CNTRY_ID,
                        GEM.LEAF_CATEG_ID,
                        GEM.SLR_ID,
                        GEM.LSTG_TYPE_CODE,
                        SUM(GEM.QTY) AS SOLD_ITEMS,
                        SUM(CAST(GEM.ITEM_PRICE * GEM.QTY * LPR.CURNCY_PLAN_RATE / SPR.CURNCY_PLAN_RATE AS DECIMAL(18,2))) AS GMV_LC,
                        SUM(CAST(GEM.ITEM_PRICE * GEM.QTY * LPR.CURNCY_PLAN_RATE AS DECIMAL(18,2))) AS GMV_PLAN
                        
            FROM
                        DW_GEM2_CMN_CK_I GEM
                        
                        /* PLAN RATES FOR LISTING CURRENCIES */
                        JOIN SSA_CURNCY_PLAN_RATE_DIM LPR
                        ON GEM.LSTG_CURNCY_ID = LPR.CURNCY_ID

                        /* SLR_RR_CURNCY AND ITS PLAN RATE TO CALCULATE GMV_LC_PLAN_AMT */
--                      JOIN DW_COUNTRIES SLR_CNTRY
--                      ON GEM.SLR_CNTRY_ID = SLR_CNTRY.CNTRY_ID 

--                      JOIN DW_REV_ROLLUP SLR_RR
--                      ON SLR_CNTRY.REV_ROLLUP_ID = SLR_RR.REV_ROLLUP_ID
                        
                        JOIN SSA_CURNCY_PLAN_RATE_DIM SPR
--                      ON SLR_RR.CURNCY_ID = SPR.CURNCY_ID
                        ON SPR.CURNCY_ID = 7

            WHERE 1=1
                        AND GEM.CK_DATE >= '2009-01-01'
                        AND GEM.CK_DATE <= '2009-12-31' 
                        AND GEM.LSTG_END_DT >=  '2009-01-01'

                        AND GEM.SLR_CNTRY_ID = 77
                        
                        AND GEM.ADJ_TYPE_ID NOT IN ( 3 , - 1 , - 7 , 5 ) 
                        AND GEM.LSTG_TYPE_CODE NOT IN ( 10,12,15 ) 
                        AND GEM.LSTG_SITE_ID NOT IN ( 223 , - 1 , - 2 , - 3 ) 
                        AND GEM.CK_WACKO_YN = 'N'

            GROUP BY 1,2,3,4,5,6,7,8) GEM
            
            JOIN ACCESS_VIEWS.DW_CAL_DT CAL 
            ON GEM.CK_DATE = CAL.CAL_DT 
            
            JOIN DW_CATEGORY_GROUPINGS CAT 
            ON GEM.LEAF_CATEG_ID = CAT.LEAF_CATEG_ID 
            AND GEM.LSTG_SITE_ID = CAT.SITE_ID

            LEFT JOIN LSTG_ITEM_CNDTN COND
            ON COND.ITEM_SITE_ID = GEM.LSTG_SITE_ID 
            AND COND.ITEM_ID = GEM.LSTG_ID
            
            LEFT JOIN DW_USEGM_HIST HIST  
            ON HIST.USER_ID=GEM.SLR_ID 
            AND HIST.USEGM_GRP_ID  = 48
            AND CASE 
                                   WHEN GEM.CK_DATE < '2009-10-11' THEN CAST('2009-10-11' AS DATE) 
                                   ELSE GEM.CK_DATE 
                        END BETWEEN HIST.BEG_DATE AND HIST.END_DATE
            
WHERE 1=1
            AND CAT.SAP_CATEGORY_ID NOT IN (5,7,41,23) 

GROUP BY 1,2,3,4,5,6,7;
