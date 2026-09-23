010000* COPYTEXT FÖR FIL PC0227F2 TILL TILLBEHÖR                                
020000* INTRODUCERADE TILLBEHÖRARTIKLAR BASERADE PÅ ÅTGÅNGAR                    
030000*                                                                         
040000* POST SKAPAS FÖR NYA TILLBEHÖR (POS=900)                                 
050000*                                                                         
060000 01  PC0234F2.                                                            
070000*                                                                         
080000     03  ARTNR                 PIC 9(8).                                  
090000*                      FÄLT SOM HÄMTATS FRÅN 1:STA ÅTGÅNG                 
100000*                      OBS GRUPP ÄR ALLTSÅ EJ DET SOM                     
110000*                      REGISTRERAS PÅ TRANS AF                            
120000     03  FKNGRUPPNR            PIC 9(4).                                  
130000     03  POS                   PIC 9(3).                                  
140000*                                                                         
150000     03  ARTTYP                PIC X(4).                                  
160000     03  PSLAG                 PIC X(2).                                  
170000     03  TYPNR                 PIC X(2).                                  
180000     03  TYPBET                PIC X(8).                                  
190000     03  AONR-T                PIC X(6).                                  
200000     03  AOUTG-T               PIC 9(2).                                  
210000     03  AOINFTID6-T           PIC 9(6).                                  
220000*                     OM ARTNR ÄR T.EX 11 BACKSPEGEL BLÅ KAN              
230000*                     'BACKSPEGEL OFÄRGAD ' VARA ARTNR 10                 
240000     03  ARTNR-OFARGAT         PIC 9(8).                                  
250000*                               N=NEUTRAL, O=OFÄRGAD F=FÄRGAD             
260000     03  FARGSTATUS            PIC X(1).                                  
270000*                      ARTIKELDATA                                        
280000     03  ARTBEN-ENG            PIC X(25).                                 
290000     03  GENANV-ENG            PIC X(25).                                 
300000     03  UPDNR-PR              PIC X(8).                                  
300010     03  LONG-BENAMN-PR        PIC X(35).                                 
300020     03  UPDNR-D1              PIC X(8).                                  
300030     03  LONG-BENAMN-D1        PIC X(35).                                 
300040     03  UPDNR-D2              PIC X(8).                                  
300050     03  LONG-BENAMN-D2        PIC X(35).                                 
300100     03  UPDNR-KU              PIC X(8).                                  
300200     03  LONG-BENAMN-KU        PIC X(35).                                 
310000     03  UPDNR-SU              PIC X(8).                                  
310100     03  LONG-BENAMN-SU        PIC X(35).                                 
320000*                                                                         
330000     03  PROJEKT               PIC X(4).                                  
340000     03  NAMN-ANSV-KU          PIC X(25).                                 
350000     03  NAMN-ANSV-SU          PIC X(25).                                 
360000     03  VIKT                  PIC 9(7).                                  
370000     03  MDSMARK               PIC X(1).                                  
380000     03  PSSID                 PIC X(5).                                  
380010*                     ARTIKELUTFÖRANDE GILTIGHET  (TILLK 2007-09)         
380100     03  ARTUTFGILT            PIC X(8).                                  
380110*                     UPPDRAGS-STATUS (FRÅN KUPP) (TILLK 2007-09)         
380200     03  UPDNR-STATUS          PIC X(6).                                  
380210*                     GENERAL SUPPLIER            (TILLK 2007-09)         
380220     03  GSDBNR                PIC X(5).                                  
380230*                     TEMP.PROD.DEVIATION STATUS  (TILLK 2007-09)         
380240     03  TPD-ST-PH1            PIC X(1).                                  
380250*                     TEMP.PROD.DEVIATION WEEK    (TILLK 2007-09)         
380260     03  TPD-WEEK-PH1          PIC X(6).                                  
380261*                          PLAN.WEEK PHASE1                               
380262     03  DPSW-P-WK-PH1         PIC X(6).                                  
380263*                          PLAN.ST PHASE1                                 
380264     03  DPSW-P-ST-PH1         PIC X(1).                                  
380265*                          ACTUAL.WEEK PHASE1                             
380266     03  DPSW-A-WK-PH1         PIC X(6).                                  
380267*                          ACTUAL.ST PHASE1                               
380268     03  DPSW-A-ST-PH1         PIC X(1).                                  
380270*                     QUAL.PLAN.WEEK PHASE1       (TILLK 2007-09)         
380280     03  QPSW-P-WK-PH1         PIC X(6).                                  
380290*                     QUAL.PLAN.ST PHASE1         (TILLK 2007-09)         
380291     03  QPSW-P-ST-PH1         PIC X(1).                                  
380292*                     QUAL.ACTUAL.WEEK PHASE1     (TILLK 2007-09)         
380293     03  QPSW-A-WK-PH1         PIC X(6).                                  
380294*                     QUAL.ACTUAL.ST PHASE1       (TILLK 2007-09)         
380295     03  QPSW-A-ST-PH1         PIC X(1).                                  
380296*                     PROD.PLAN.WEEK PHASE2       (TILLK 2007-09)         
380297     03  PPSW-P-WK-PH2         PIC X(6).                                  
380298*                     PROD.PLAN.ST PHASE2         (TILLK 2007-09)         
380299     03  PPSW-P-ST-PH2         PIC X(1).                                  
380300*                     PROD.ACTUAL.WEEK PHASE2     (TILLK 2007-09)         
380310     03  PPSW-A-WK-PH2         PIC X(6).                                  
380320*                     PROD.ACTUAL.ST PHASE2       (TILLK 2007-09)         
380330     03  PPSW-A-ST-PH2         PIC X(1).                                  
380340*                     CAPACITY PLAN.WEEK PHASE3   (TILLK 2007-09)         
380350     03  CPSW-P-WK-PH3         PIC X(6).                                  
380360*                     CAPACITY PLAN.ST PHASE3     (TILLK 2007-09)         
380370     03  CPSW-P-ST-PH3         PIC X(1).                                  
380380*                     CAPACITY ACTUAL.WEEK PHASE3 (TILLK 2007-09)         
380390     03  CPSW-A-WK-PH3         PIC X(6).                                  
380391*                     CAPACITY ACTUAL.ST PHASE3   (TILLK 2007-09)         
380392     03  CPSW-A-ST-PH3         PIC X(1).                                  
380400*                                                                         
380500     03  FILLER                PIC X(25).                                 
380600*                                                                         
390000*** END OF VILMAII-COPY LENGTH= 493                                       
