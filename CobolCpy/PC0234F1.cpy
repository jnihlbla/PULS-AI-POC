000100* COPYTEXT FÖR FIL PC0227F1 TILL TIKO                                     
000200* INTRODUCERADE ARTIKLAR BASERADE PÅ ÅTGÅNGAR                             
000300*                                                                         
000400* COPYTEXTEN NÅGOT ÄNDRAD 2003-01-22 EFTER PROJEKTMÖTE                    
000500*                                                                         
000600* -POSTTYP           KOMMENTAR ÄNDRAD. MAN VILL HA ENKLARE NAMN           
000700* -POST              BORTTAGET                                            
000800* -DATUM-STATUS      INLAGT                                               
000900* -FORDNR            MAN FÅR EJ BARA PRINTFÄLT UTAN GRUNDDATA             
001000* -ALLMÄNT           FÖRKLARANDE KOMMENTARER INLAGDA                      
001100*                                                                         
001200* -LOGIKÄNDRING   MAN BEHÖVER SE NÄR EN ARTIKEL ÄNDRAR SIG SÅ             
001300*                 ATT DEN INTE LÄNGRE ÄÄR VANLIG ÅTGÅNG UTAN              
001400*                 BLIR EN ARTIKEL SOM INGÅR I 'BILDSYSTEM'.               
001500*                 I SÅDANT FALL SKAPAS 2 POSTER                           
001600*                 = EN MED BILDSYSTEMMÄRKNING BLANKT                      
001700*                 = EN MED BILDSYSTEMMÄRKNING ='B'                        
001800*                (OM TIKO VILL KAN VI DELA DESSA PÅ 2 FILER)              
001900*                                                                         
002000 01  PC0234F1.                                                            
002100*                      POSTTYP   'KD1'                                    
002200     03  POSTTYP               PIC X(3).                                  
002300*                                                                         
002400*                      ARTIKEL SOM ÄR NY. KAN VARA ETT FÖRSERIENR         
002500*                      - NY LEVERANSENHET                                 
002600*                      - OFÄRGAD (VIT ENLIGT FORD) ELLER FÄRGAD           
002700*                      - NY INGÅENDE                                      
002800*                      - NY MODUL                                         
002900     03  ARTNR                 PIC 9(8).                                  
003000*                      FÄLT SOM HÄMTATS FRÅN 1:STA ÅTGÅNG                 
003100*                        OBS GRUPP ÄR ALLTSÅ EJ DET SOM                   
003200*                            REGISTRERAS PÅ TRANS AF                      
003300*                                 (GRUPP      I TIKO)                     
003400*                                                                         
003500     03  ARTNR-SOP             PIC 9(8).                                  
003600     03  FKNGRUPPNR            PIC 9(4).                                  
003700*                                 (POS 0 ARTTYP REGISTRERAS               
003800*                                  I TIKO SOM SMARK)                      
003900     03  POS                   PIC 9(3).                                  
004000     03  ARTTYP                PIC X(4).                                  
004100*                          PSLAG OCH TYPNR KAN ÖVERSÄTTAS                 
004200*                          TILL TYPBET GENOM SUB PC14GXXX                 
004300     03  PSLAG                 PIC X(2).                                  
004400     03  TYPNR                 PIC X(2).                                  
004500*                                 (AEONR-B    I TIKO)                     
004600     03  AONR-T                PIC X(6).                                  
004700     03  AOUTG-T               PIC 9(2).                                  
004800*                                 (AAVV-PROD-INFORANDE I TIKO)            
004900     03  AOINFTID6-T           PIC 9(6).                                  
005000*                      NORMALT X, MEN KAN OCSÅ VARA                       
005100*                      T (TILLS.MAT) E (ERSART) MM                        
005200     03  LEVMARK               PIC X(1).                                  
005300*                      NORMALT BLANKT. A INNEBÄR AVVEKELSE                
005400     03  AVVIKELSEKOD          PIC X.                                     
005500*                      FÄLT SOM HÄMTAS FRÅN SISTA ÅTGÅNG                  
005600     03  AONR-U                PIC X(6).                                  
005700     03  AOUTG-U               PIC 9(2).                                  
005800*                      FÖR POSTER UTAN ANOR-U                             
005900*                      ÄR UTG-TID AOINFTID6-U = 999952                    
006000*                      AAMMDD-UTG-ART I TIKO                              
006100     03 AOINFTID6-U            PIC 9(6).                                  
006200*                      FÄLT BASERADE PÅ FÄRGTABELL/FÄRGMÄRKNING           
006300*                            (ARTNR I SEGM ACD222 I TIKO)                 
006400     03  ARTNR-OFARGAT         PIC 9(8).                                  
006500*                               N=NEUTRAL, O=OFÄRGAD F=FÄRGAD             
006600*                                 (MARK-OFARG I TIKO)                     
006700     03  FARGSTATUS            PIC X(1).                                  
006800*                      LEVERANSENHET VARMED DENNA ARTIKEL                 
006900*                      INTRODUCERAS (NOLL OM ARTNR=LEVENHET)              
007000*                            (ARTNR-LEVENH I SEGM ACD227 I TIKO)          
007100     03  ARTNR-LEV             PIC 9(8).                                  
007200*                      ARTIKELDATA                                        
007300     03  ARTBEN                PIC X(25).                                 
007400     03  GENANV                PIC X(25).                                 
007500     03  ARTBEN-ENG            PIC X(25).                                 
007600     03  GENANV-ENG            PIC X(25).                                 
007700*                          STYCK    BLANKT                                
007800*                          G      = GRAM                                  
007900*                          MM     = MILIMETRE                             
008000*                          KVCM   = SQUARE CENTIMETRE                     
008100*                          ML     = MILILITRE                             
008200     03  ARTSORT               PIC X(4).                                  
008300*                         3 FÄLT NEDAN ERSÄTTER DAGENS DOKPLIKT           
008400*                            (FKLASS I TIKO)                              
008500     03  DOKPLIKT-D            PIC X(2).                                  
008600     03  DOKPLIKT-T            PIC X(1).                                  
008700     03  DOKPLIKT-Y            PIC X(1).                                  
008800*                                                                         
008900     03  ARTUTF                PIC X(3).                                  
009000     03  ARTUTF-SOP            PIC X(3).                                  
009100     03  HDOKUTG               PIC 9(2).                                  
009200*                          KAN VARA KOMPUTV,ANSK,PROD,VERKT               
009300*                          (I SPECIALFALL EXP)                            
009400     03  ARTUTFGILT            PIC X(8).                                  
009500*                            DATUM FÖR FASTSTÄLLANDE AV HUVUDDOK          
009600*                            AAAAVVD                                      
009700     03  DATUM-STATUS          PIC 9(7).                                  
009800*                            SUPERCEDED PART                              
009900*                            (ARTNR I SEGM ACD222 I TIKO)                 
010000     03  ARTNR-GAMMALT         PIC 9(8).                                  
010100*                      UPPGIFTER SOM ÄR TVEKSAMMA. JOL TÄNKER             
010200*                            (RITNR I TIKO)                               
010300     03  RITNNR                PIC X(8).                                  
010400*                            (UTGAVA-AKT I TIKO)                          
010500     03  DOKUTG                PIC X(3).                                  
010600*                            (AAVV-UTGAVA-AKT I TIKO)                     
010700     03  AAAAMMDD-DOKUTG       PIC X(8).                                  
010800     03  BILDSYSTMARKN         PIC X.                                     
010900*                                                                         
011000*FORDS NR SKRIVS UT SOM PPPP-BBBBBBBB-SSSSSSSS?                           
011100*                            (FORD-INFO I SEGM ACD223 I TIKO)             
011200     03 FORD-DATA.                                                        
011300       05 PARTNO-EX.                                                      
011400         07 PARTNO-PREF-EX                  PIC X(6).                     
011500*              PARTBASE-FORD           PV3402                             
011600         07 PARTNO-BASE-EX                  PIC X(8).                     
011700*              PARTPREF-FORD           PV3401                             
011800         07 PARTNO-SUF-EX                   PIC X(8).                     
011900*              PARTVAR-FORD            PV3403                             
012000*              AT PRESENT, ONLY POS 1 IN PART-VERS IS USED?               
012100       05 OWNER                             PIC X(5).                     
012200*                                                                         
012300       05 COMMONMARK                        PIC X(1).                     
012400*                                                                         
012500*                                    (UPPDRAG I TIKO)                     
012600     03  UPDNR-KU              PIC X(8).                                  
012700*                                    (UPPDRAG-S I TIKO)                   
012800     03  UPDNR-SU              PIC X(8).                                  
012900*                                                                         
013000     03  PROJEKT               PIC X(4).                                  
013100*                                    J/N                                  
013200     03  MDSMARK               PIC X(1).                                  
013300     03  PSSID                 PIC X(5).                                  
013400     03  AEOORS                PIC X(5).                                  
013400     03  DOKPLIKT-C            PIC X(2).                                  
013500     03  FILLER                PIC X(37).                                 
013600*** END OF VILMAII-COPY LENGTH= 338 OLD LENGTH= 338                       
