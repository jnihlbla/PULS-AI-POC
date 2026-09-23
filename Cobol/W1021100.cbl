000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1021100.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   APRIL 1990.                                              
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING OCH NYUPPLÄGG AV STRUKTURHUVUD.                      
001100*        BORTTAG AV BEFINTLIG STRUKTUR.                                   
001200*        KOPIERING AV BEFINTLIG STRUKTUR TILL NY STRUKTUR.                
001300*                                                                         
001400*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001500*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
001600*        PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
001700*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET LÄSER      WDF5                                       
002000*                                                                         
002100*        ÄT SPLIT 930402 BL                                               
002200*           - ÄNDRING GODKÄNDA PRODUKTSLAG                                
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W1T211                                              
002600*        MID:         W1I21101                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W1O21101                                            
003000*                                                                         
003100*    SUBPGM.                                                              
003200*        FELLOG                                                           
003300*        CBLTDLI                                                          
003400*        WORKDAY                                                          
003500*        W009VADD                                                         
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -COPY WY2000W1                                                       
004300     SKIP3                                                                
004400 77  IDPGM                   PIC X(8)    VALUE 'W1021100'.                
004500 77  JA                      PIC X       VALUE 'J'.                       
004600 77  NEJ                     PIC X       VALUE 'N'.                       
004700 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
004800 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004900 77  INDX2                   PIC S9(9)   VALUE +0   COMP SYNC.            
005000 77  MAX-IDAO                PIC S9      VALUE +5.                        
005100 77  MAX-TABELL-LAENGD       PIC S9(3)   VALUE +20.                       
005200 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +557 COMP SYNC.            
005300 77  IDLEVNR-ART-WS          PIC X(5)    VALUE SPACE.                     
005400 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
005500 77  IDARTNR-KONVERTERAT-WS  PIC 9(9).                                    
005600 77  IDARTNR-NYUPPL-WS       PIC 9(9).                                    
005700 77  IDARTNR-NYUPPL-KONVERTERAT-WS  PIC 9(9).                             
005800 77  IDARTNR2-WS             PIC 9(9).                                    
005900 77  IDSKYLT-WS              PIC X(3).                                    
006000 77  KDPRODSL-WS             PIC 9(3).                                    
006100 77  KDBENHOM-WS             PIC 9(1).                                    
006200 77  KDBENHOM-SPAR           PIC 9(1).                                    
006300 77  KDBENHOM-RASA-SPAR      PIC 9(1).                                    
006400 77  IDFKNGRP-WS             PIC X(4).                                    
006500 77  BEART-SPAR              PIC X(25).                                   
006600 77  BEART-RASA-SPAR         PIC X(25).                                   
006700 77  BEART-SVE-SPAR          PIC X(25).                                   
006800                                                                          
006900 77  INDATA-SW               PIC X       VALUE 'J'.                       
007000   88  INDATA-OK                         VALUE 'J'.                       
007100   88  INDATA-FEL                        VALUE 'N'.                       
007200                                                                          
007300 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
007400   88  NYCKLAR-OK                        VALUE 'J'.                       
007500   88  NYCKLAR-FEL                       VALUE 'N'.                       
007600                                                                          
007700 77  ALLT-SW                 PIC X       VALUE 'N'.                       
007800   88  ALLT-OK                           VALUE 'J'.                       
007900                                                                          
008000 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
008100   88  EGEN-MID                          VALUE '1211'.                    
008200   88  GODK-MID                          VALUE '1211'                     
008300                                               '1212' '1213'              
008400                                               '1214' '1215'.             
008500   88  MID-MED-IDSKYLT                   VALUE '1211' '1212'              
008600                                               '1213' '1214'.             
008700                                                                          
008800 77  INGAAR-I-KOPIERAD-SATS-SW PIC X.                                     
008900   88  INGAAR-I-KOPIERAD-SATS            VALUE 'J'.                       
009000   88  INGAAR-EJ-I-KOPIERAD-SATS         VALUE 'N'.                       
009100                                                                          
009200 77  SATSTABELL-SLUT-SW      PIC X.                                       
009300   88  SATSTABELL-SLUT                   VALUE 'J'.                       
009400                                                                          
009500 77  KDPRODSL-GODK-WS        PIC 9(2).                                    
009600   88  KDPRODSL-GODK                     VALUE 11 13 14 15 16             
009700                                               17 18 19                   
010100                                               71 72 73 74                
010200                                               21 23 24 25 26             
010300                                               27 28 29.                  
010400   EJECT                                                                  
010500*************************************************                         
010600*             UPPDATERINGS-TYPER                *                         
010700*************************************************                         
010800                                                                          
010900 77  UPDATE-TYP              PIC X.                                       
011000   88  UPDATE-AENDRA                     VALUE 'Ä'.                       
011100   88  UPDATE-BORTTAG                    VALUE 'B'.                       
011200   88  UPDATE-NYUPPL                     VALUE 'N'.                       
011300   88  UPDATE-NYUPPL-KOPIERING           VALUE 'K'.                       
011400   EJECT                                                                  
011500*      --- VALID IDDC CODES                                               
011600*                                                                         
011700*01    -COPY WWDCKONS                                                     
011800*01    -COPY WWDC99                                                       
011900       EJECT                                                              
012000                                                                          
012100*************************************************                         
012200*         STRUKTURNR-TYP PÅ RASA                *                         
012300*************************************************                         
012400                                                                          
012500********* FÖR IDARTNR-NYCKEL ********************                         
012600                                                                          
012700 77  STRUKTURNR-TYP          PIC X.                                       
012800   88  STRUKTURNR-FINNS                  VALUE 'F'.                       
012900   88  STRUKTURNR-SAKNAS                 VALUE 'S'.                       
013000   88  STRUKTURNR-SPAERRAT               VALUE 'P'.                       
013100   88  STRUKTURNR-BORTTAGET              VALUE 'B'.                       
013200                                                                          
013300 77  FINNS-OKONVERTERAT-SW   PIC X.                                       
013400   88  STRUKTURNR-FINNS-OKONVERTERAT     VALUE 'J'.                       
013500                                                                          
013600 77  FINNS-KONVERTERAT-SW    PIC X.                                       
013700   88  STRUKTURNR-FINNS-KONVERTERAT      VALUE 'J'.                       
013800                                                                          
013900********* FÖR IDARTNR-NYUPPLÄGG *****************                         
014000                                                                          
014100 77  STRUKTURNR-NY-TYP       PIC X.                                       
014200   88  STRUKTURNR-NY-FINNS               VALUE 'F'.                       
014300                                                                          
014400   EJECT                                                                  
014500                                                                          
014600*************************************************                         
014700*         STRUKTURNR-TYP PÅ ARTREG              *                         
014800*************************************************                         
014900                                                                          
015000********* FÖR IDARTNR-NYCKEL ********************                         
015100                                                                          
015200 77  STRUKTURNR-PAA-ARTREG   PIC X.                                       
015300   88  STRUKTURNR-FINNS-PAA-ARTREG       VALUE 'F'.                       
015400                                                                          
015500********* FÖR IDARTNR-NYUPPLÄGG *****************                         
015600                                                                          
015700 77  STRUKTURNR-NY-PAA-ARTREG   PIC X.                                    
015800   88  STRUKTURNR-NY-FINNS-PAA-ARTREG    VALUE 'F'.                       
015900                                                                          
016000   EJECT                                                                  
016100                                                                          
016200 01  AENDRING                PIC X       VALUE 'Ä'.                       
016300 01  BORTTAG                 PIC X       VALUE 'B'.                       
016400 01  NYUPPLAEGG              PIC X       VALUE 'N'.                       
016500 01  NYUPPLAEGG-KOPIERING    PIC X       VALUE 'K'.                       
016600                                                                          
016700 01  FINNS                   PIC X       VALUE 'F'.                       
016800 01  SAKNAS                  PIC X       VALUE 'S'.                       
016900 01  SPAERRAT                PIC X       VALUE 'P'.                       
017000 01  BORTTAGET               PIC X       VALUE 'B'.                       
017100                                                                          
017200 01  DAGENS-DATUM            PIC 9(6).                                    
017300 01  FILLER REDEFINES DAGENS-DATUM.                                       
017400   03  DATUM-AAMM            PIC 9(4).                                    
017500   03  DATUM-DD              PIC 9(2).                                    
017600                                                                          
017700 01  DATUM-AAVV-MELLANLAGR   PIC 9(4).                                    
017800                                                                          
017900 01  W009VADD-AREA.                                                       
018000   03  W009VADD-AAVV         PIC S9(5)   COMP-3.                          
018100   03  ANTAL                 PIC S9(3)   COMP-3.                          
018200                                                                          
018300     EJECT                                                                
018400****************** TABELLER ****************************                  
018500                                                                          
018600 01  SATSTABELL.                                                          
018700   03  SATSNR     OCCURS 20 PIC S9(9)   COMP-3.                           
018800                                                                          
018900     EJECT                                                                
019000 01  MEDDELANDEN.                                                         
019100   03  MED-1.                                                             
019200       05  FILLER            PIC X(40)   VALUE                            
019300           'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                       
019400       05  FILLER            PIC X(40)   VALUE                            
019500           'PF11 AND NO INPUT                    '.                       
019600   03  FILLER REDEFINES MED-1.                                            
019700       05  MED1 OCCURS 2     PIC X(40).                                   
019800                                                                          
019900   03  MED-2.                                                             
020000       05  FILLER            PIC X(40)   VALUE                            
020100           'REGISTRERING UTFÖRD. STRUKTUR EJ KLAR'.                       
020200       05  FILLER            PIC X(40)   VALUE                            
020300           'UPDATED. STRUCTURE NOT COMPLETE      '.                       
020400   03  FILLER REDEFINES MED-2.                                            
020500       05  MED2 OCCURS 2     PIC X(40).                                   
020600                                                                          
020700   03  MED-3.                                                             
020800       05  FILLER            PIC X(60)   VALUE                            
020900           'STRUKTUR KAN EJ TAS BORT. ÄR UNDER BEARBETNING'.              
021000       05  FILLER            PIC X(60)   VALUE                            
021100           'DELETE NOT POSSIBLE. STRUCTURE IN USE'.                       
021200   03  FILLER REDEFINES MED-3.                                            
021300       05  MED3 OCCURS 2     PIC X(60).                                   
021400                                                                          
021500   03  MED-4.                                                             
021600       05  FILLER            PIC X(40)   VALUE                            
021700           'RADER FINNS REGISTRERADE '.                                   
021800       05  FILLER            PIC X(40)   VALUE                            
021900           'STRUCTURE LINES EXIST    '.                                   
022000   03  FILLER REDEFINES MED-4.                                            
022100       05  MED4 OCCURS 2     PIC X(40).                                   
022200                                                                          
022300   03  MED-5.                                                             
022400       05  FILLER            PIC X(40)   VALUE                            
022500           'UPPDATERING UTFÖRD'.                                          
022600       05  FILLER            PIC X(40)   VALUE                            
022700           'UPDATED                  '.                                   
022800   03  FILLER REDEFINES MED-5.                                            
022900       05  MED5 OCCURS 2     PIC X(40).                                   
023000                                                                          
023100   03  MED-6.                                                             
023200       05  FILLER            PIC X(40)   VALUE                            
023300           'TRYCK PF11 FÖR UPPDATERING'.                                  
023400       05  FILLER            PIC X(40)   VALUE                            
023500           'PRESS PF11 TO UPDATE     '.                                   
023600   03  FILLER REDEFINES MED-6.                                            
023700       05  MED6 OCCURS 2     PIC X(40).                                   
023800                                                                          
023900   03  MED-7.                                                             
024000       05  FILLER            PIC X(40)   VALUE                            
024100           'STRUKTUR ÄR UNDER BEARBETNING'.                               
024200       05  FILLER            PIC X(40)   VALUE                            
024300           'STRUCTURE IS IN USE      '.                                   
024400   03  FILLER REDEFINES MED-7.                                            
024500       05  MED7 OCCURS 2     PIC X(40).                                   
024600                                                                          
024700   03  MED-8.                                                             
024800       05  FILLER            PIC X(40)   VALUE                            
024900           'STRUKTUR BORTTAGSMÄRKT'.                                      
025000       05  FILLER            PIC X(40)   VALUE                            
025100           'STRUCTURE IS MARKED TO BE DELETED'.                           
025200   03  FILLER REDEFINES MED-8.                                            
025300       05  MED8 OCCURS 2     PIC X(40).                                   
025400                                                                          
025500   03  MED-9.                                                             
025600       05  FILLER            PIC X(60)   VALUE                            
025700      'STRUKTUR KAN EJ KOPIERAS. INNEHÅLLER INMATAT STRUKTURNR'.          
025800       05  FILLER            PIC X(60)   VALUE                            
025900       'UPDATE NOT POSSIBLE'.                                             
026000   03  FILLER REDEFINES MED-9.                                            
026100       05  MED9 OCCURS 2     PIC X(60).                                   
026200                                                                          
026300   03  MED-10.                                                            
026400       05  FILLER            PIC X(40)   VALUE                            
026500           'STRUKTUR FINNS REDAN PÅ BASEN'.                               
026600       05  FILLER            PIC X(40)   VALUE                            
026700           'STRUCTURE ALREADY REGISTRED'.                                 
026800   03  FILLER REDEFINES MED-10.                                           
026900       05  MED10 OCCURS 2     PIC X(40).                                  
027000                                                                          
027100   03  MED-11.                                                            
027200       05  FILLER            PIC X(60)   VALUE                            
027300           'ARTIKEL ERSÄTTNINGSMÄRKT PÅ ARTIKELREGISTRET'.                
027400       05  FILLER            PIC X(60)   VALUE                            
027500           'THIS PART IS SUPERSEDED'.                                     
027600   03  FILLER REDEFINES MED-11.                                           
027700       05  MED11 OCCURS 2     PIC X(60).                                  
027800                                                                          
027900   03  MED-12.                                                            
028000       05  FILLER            PIC X(60)   VALUE                            
028100           'STRUKTUR KAN EJ KOPIERAS. ÄR UNDER BEARBETNING'.              
028200       05  FILLER            PIC X(60)   VALUE                            
028300           'STRUCTURE IS IN USE'.                                         
028400   03  FILLER REDEFINES MED-12.                                           
028500       05  MED12 OCCURS 2    PIC X(60).                                   
028600                                                                          
028700   03  MED-13.                                                            
028800       05  FILLER            PIC X(60)   VALUE                            
028900           'STRUKTURNR FINNS REGISTRERAD SOM RAD'.                        
029000       05  FILLER            PIC X(60)   VALUE                            
029100           'STRUCTURE EXISTS AS A STRUCTURE LINE'.                        
029200   03  FILLER REDEFINES MED-13.                                           
029300       05  MED13 OCCURS 2     PIC X(60).                                  
029400                                                                          
029500   03  MED-14.                                                            
029600       05  FILLER            PIC X(60)   VALUE                            
029700           'DU HAR STRUKTUR UNDER BEARBETNING'.                           
029800       05  FILLER            PIC X(60)   VALUE                            
029900           'YOU HAVE STRUCTURE IN USE'.                                   
030000   03  FILLER REDEFINES MED-14.                                           
030100       05  MED14 OCCURS 2    PIC X(60).                                   
030200                                                                          
030300   03  MED-15.                                                            
030400       05  FILLER            PIC X(60)   VALUE                            
030500           'ARTIKEL RENSAD PÅ ARTIKELREGISTRET'.                          
030600       05  FILLER            PIC X(60)   VALUE                            
030700           'PART NUMBER DELETED              '.                           
030800   03  FILLER REDEFINES MED-15.                                           
030900       05  MED15 OCCURS 2    PIC X(60).                                   
031000                                                                          
031100   03  MED-16.                                                            
031200       05  FILLER            PIC X(60)   VALUE                            
031300           'GÄLLANDE RADER FINNS'.                                        
031400       05  FILLER            PIC X(60)   VALUE                            
031500           'VALID STRUCTURE LINES EXIST'.                                 
031600   03  FILLER REDEFINES MED-16.                                           
031700       05  MED16 OCCURS 2    PIC X(60).                                   
031800                                                                          
031900   03  MED-17.                                                            
032000       05  FILLER            PIC X(60)   VALUE                            
032100         'STRUKTUR FINNS REGISTRERAD. ENDAST RADER KAN KOPIERAS'.         
032200       05  FILLER            PIC X(60)   VALUE                            
032300          'ONLY STRUCTURE LINES CAN BE COPIED '.                          
032400   03  FILLER REDEFINES MED-17.                                           
032500       05  MED17 OCCURS 2    PIC X(60).                                   
032600                                                                          
032700   03  MED-18.                                                            
032800       05  FILLER            PIC X(60)   VALUE                            
032900           'UPPDATERING UTFÖRD. RADER KOPIERADE'.                         
033000       05  FILLER            PIC X(60)   VALUE                            
033100          'UPDATED. STRUCTURE LINES COPIED'.                              
033200   03  FILLER REDEFINES MED-18.                                           
033300       05  MED18 OCCURS 2    PIC X(60).                                   
033400                                                                          
033500   03  FEL-1.                                                             
033600       05  FILLER            PIC X(40)   VALUE                            
033700           'STRUKTUR SAKNAS'.                                             
033800       05  FILLER            PIC X(40)   VALUE                            
033900           'STRUCTURE IS MISSING'.                                        
034000   03  FILLER REDEFINES FEL-1.                                            
034100       05  FEL1 OCCURS 2     PIC X(40).                                   
034200                                                                          
034300   03  FEL-2.                                                             
034400       05  FILLER            PIC X(40)   VALUE                            
034500           'KORRIGERA UPPLYSTA FÄLT'.                                     
034600       05  FILLER            PIC X(40)   VALUE                            
034700           'CORRECT HIGHLIGHTED FIELDS'.                                  
034800   03  FILLER REDEFINES FEL-2.                                            
034900       05  FEL2 OCCURS 2     PIC X(40).                                   
035000                                                                          
035100   03  FEL-3.                                                             
035200       05  FILLER            PIC X(40)   VALUE                            
035300           'STRUKTURNR EJ NUMERISKT'.                                     
035400       05  FILLER            PIC X(40)   VALUE                            
035500           'STRUCTURE NUMBER NOT NUMERIC'.                                
035600   03  FILLER REDEFINES FEL-3.                                            
035700       05  FEL3 OCCURS 2     PIC X(40).                                   
035800                                                                          
035900   03  FEL-4.                                                             
036000       05  FILLER            PIC X(40)   VALUE                            
036100           'NYCKLAR FEL      '.                                           
036200       05  FILLER            PIC X(40)   VALUE                            
036300           'WRONG KEYS       '.                                           
036400   03  FILLER REDEFINES FEL-4.                                            
036500       05  FEL4 OCCURS 2     PIC X(40).                                   
036600                                                                          
036700   03  FEL-5.                                                             
036800       05  FILLER            PIC X(40)   VALUE                            
036900           'STRUKTURNR FELAKTIGT'.                                        
037000       05  FILLER            PIC X(40)   VALUE                            
037100           'STRUCTURE NUMBER NOT CORRECT'.                                
037200   03  FILLER REDEFINES FEL-5.                                            
037300       05  FEL5 OCCURS 2     PIC X(40).                                   
037400                                                                          
037500   03  FEL-6.                                                             
037600       05  FILLER            PIC X(40)   VALUE                            
037700           'UPPDATERING EJ TILLÅTEN'.                                     
037800       05  FILLER            PIC X(40)   VALUE                            
037900           'UPDATE NOT ALLOWED'.                                          
038000   03  FILLER REDEFINES FEL-6.                                            
038100       05  FEL6 OCCURS 2     PIC X(40).                                   
038200                                                                          
038300   03  FEL-7.                                                             
038400       05  FILLER            PIC X(40)   VALUE                            
038500           'MATA IN NYA NYCKLAR    '.                                     
038600       05  FILLER            PIC X(40)   VALUE                            
038700           'ENTER NEW KEYS    '.                                          
038800   03  FILLER REDEFINES FEL-7.                                            
038900       05  FEL7 OCCURS 2     PIC X(40).                                   
039000                                                                          
039100     EJECT                                                                
039200 01  GENERELLA-SUBPROGRAM.                                                
039300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
039400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
039500   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
039600   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
039700   03  W009VADD              PIC X(8)    VALUE 'W009VADD'.                
039800   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
039900     EJECT                                                                
040000*01   -COPY WWLAND03                                                      
040100     EJECT                                                                
040200*01   -COPY WMEDAREA                                                      
040300     EJECT                                                                
040400*01   -COPY WORKAREA                                                      
040500     EJECT                                                                
040600*                     ****   PARAMETRAR TILL W005INIT                     
040700*01   -COPY WMSGINIT                                                      
040800     EJECT                                                                
040900******************************************************************        
041000*                                                                         
041100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041200*                                                                         
041300 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
041400     SKIP3                                                                
041500*01  MID -COPY W1I21101                                                   
041600     EJECT                                                                
041700*01    -COPY WMSGAREA                                                     
041800     EJECT                                                                
041900*  03  MOD -COPY W1O21101  -RED MSG-AREA.                                 
042000     EJECT                                                                
042100*01    -COPY WMFSAREA                                                     
042200     EJECT                                                                
042300******************************************************************        
042400*                                                                         
042500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042600*                                                                         
042700 01  IMS-WS.                                                              
042800   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
042900     SKIP3                                                                
043000*                        **** STATUS-KOD FRÅN IMS                         
043100   03  STATUS-WS             PIC XX.                                      
043200     88  SEGMENT-FINNS                   VALUE '  '.                      
043300     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
043400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
043500     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
043600                                                                          
043700*         **** STATUS-KODER SOM ANVÄNDS VID KOPIERING                     
043800   03  STATUS-NOT-WS         PIC XX.                                      
043900     88  NOTSEGMENT-SLUT                 VALUE 'GE'.                      
044000                                                                          
044100   03  STATUS-ART-WS         PIC XX.                                      
044200     88  RADSEGMENT-SLUT                 VALUE 'GE'.                      
044300                                                                          
044400   03  GODK-STATUSKODER.                                                  
044500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044600                                                                          
044700 01  NYCKLAR-TILL-DLI.                                                    
044800   03  W-IDARTNR-X.                                                       
044900     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
045000                                                                          
045100   03  W-IDARTNR-I-X.                                                     
045200     05  W-IDARTNR-I         PIC S9(9)   VALUE ZERO  COMP-3.              
045300                                                                          
045400   03  W-IDARTNR-KONV-MIN-X.                                              
045500     05  W-IDARTNR-KONV-MIN  PIC S9(9)   VALUE +100000000 COMP-3.         
045600                                                                          
045700   03  W-IDARTNR-KONV-MAX-X.                                              
045800     05  W-IDARTNR-KONV-MAX  PIC S9(9)   VALUE +999999999 COMP-3.         
045900                                                                          
046000   03  W-IDARTNR-MIN-X.                                                   
046100     05  W-IDARTNR-MIN       PIC S9(9)   VALUE +1 COMP-3.                 
046200                                                                          
046300   03  W-IDARTNR-MAX-X.                                                   
046400     05  W-IDARTNR-MAX       PIC S9(9)   VALUE +99999999  COMP-3.         
046500                                                                          
046600   03  W-IDSKYLT-X.                                                       
046700     05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                     
046800                                                                          
046900   03  W-KDNOTTYP-X.                                                      
047000     05  W-KDNOTTYP          PIC  S9(1)  VALUE ZERO COMP-3.               
047100                                                                          
047200   03  W-BEART-X.                                                         
047300     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
047400                                                                          
047500   03  W-IDLEVNR-X.                                                       
047600     05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                      
047700                                                                          
047800   03  W-BELEVART-X.                                                      
047900     05  W-BELEVART          PIC  X(30)  VALUE SPACE.                     
048000                                                                          
048100   03  W-IDBENR-X.                                                        
048200     05  W-IDBENR            PIC  S9(1)  VALUE ZERO  COMP-3.              
048300                                                                          
048400   03  W-KDSTRRAD-X.                                                      
048500     05  W-KDSTRRAD          PIC  X(1)   VALUE SPACE.                     
048600                                                                          
048700   03  W-IDRADNR-X.                                                       
048800     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
048900                                                                          
049000   03  W-IDUSER-X.                                                        
049100     05  W-IDUSER            PIC  X(8)   VALUE SPACE.                     
049200                                                                          
049300   03  W-WDGXKEY-X.                                                       
049400     05  W-IDHTYP            PIC  X(4)   VALUE SPACE.                     
049500     05  W-LOW-VALUE         PIC  X(26)  VALUE SPACE.                     
049600                                                                          
049700 01    SSA1                  PIC X(96).                                   
049800 01    SSA2                  PIC X(96).                                   
049900 01    SSA3                  PIC X(64).                                   
050000     EJECT                                                                
050100*                            IMS FUNKTIONSKODER                           
050200*01    -COPY W0003                                                        
050300     EJECT                                                                
050400*    -------------     DLI INPUT-OUTPUT AREA                              
050500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
050600                                                                          
050700 01  DLI-IO-AREA.                                                         
050800   03  IO-AREA               PIC X(928)  VALUE SPACE.                     
050900     SKIP3                                                                
051000*  03  WLBENA01  -COPY WDD301  -PRE BENA01-  -RED IO-AREA.                
051100     EJECT                                                                
051200*  03  WLBENA11  -COPY WDD311  -PRE BENA11-  -RED IO-AREA.                
051300     EJECT                                                                
051400*  03  WDF501    -COPY WDF501                -RED IO-AREA.                
051500     EJECT                                                                
051600*  03  WDF502    -COPY WDF502                -RED IO-AREA.                
051700     EJECT                                                                
051800*  03  WLARTC01  -COPY WDK601                -RED IO-AREA.                
051900     EJECT                                                                
052000*  03  WLARTC11  -COPY WDK611                -RED IO-AREA.                
052100     EJECT                                                                
052200*  03  WLARTC25  -COPY WDK625                -RED IO-AREA.                
052300     EJECT                                                                
052400*  03  WLSATB01  -COPY WDJ101  -PRE SATB01-  -RED IO-AREA.                
052500     EJECT                                                                
052600*  03  WLSATB11  -COPY WDJ111  -PRE SATB11-  -RED IO-AREA.                
052700     EJECT                                                                
052800*  03  WLSATB22  -COPY WDJ122  -PRE SATB22-  -RED IO-AREA.                
052900     EJECT                                                                
053000*  03  WLXXAZ11  -COPY WDGX1152 -PRE XXAZ11-  -RED IO-AREA.               
053100     EJECT                                                                
053200                                                                          
053300   03  WLSATB-CSEQ REDEFINES IO-AREA.                                     
053400*      05 WLSATB11 -COPY WDJ111 -PRE SATB11C-                             
053500         SKIP3                                                            
053600*      05 WLSATB01 -COPY WDJ101 -PRE SATB01C-                             
053700         EJECT                                                            
053800                                                                          
053900*    -------------     DLI INPUT-OUTPUT AREA-2                            
054000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA-2'.           
054100                                                                          
054200 01  DLI-IO-AREA2.                                                        
054300   03  IO-AREA2             PIC X(240)  VALUE SPACE.                      
054400*  03  WLSATB01  -COPY WDJ101  -PRE SATB01I-  -RED IO-AREA2.              
054500     EJECT                                                                
054600*  03  WLSATB11  -COPY WDJ111  -PRE SATB11I-  -RED IO-AREA2.              
054700     EJECT                                                                
054800*  03  WLSATB22  -COPY WDJ122  -PRE SATB22I-  -RED IO-AREA2.              
054900     EJECT                                                                
055000 LINKAGE SECTION.                                                         
055100*01    -COPY W0009     -PRE MSG-                                          
055200     EJECT                                                                
055300*01    -COPY W0008     -PRE USEA-                                         
055400     05  FILLER              PIC X.                                       
055500     EJECT                                                                
055600*01    -COPY W0008     -PRE WDF5-                                         
055700     05  FILLER              PIC X.                                       
055800     EJECT                                                                
055900*01    -COPY W0008     -PRE ARTC-                                         
056000     05  FILLER              PIC X.                                       
056100     EJECT                                                                
056200*01    -COPY W0008     -PRE SATB-                                         
056300     05  FILLER              PIC X.                                       
056400     EJECT                                                                
056500*01    -COPY W0008     -PRE SATB-I-                                       
056600     05  FILLER              PIC X.                                       
056700     EJECT                                                                
056800*01    -COPY W0008     -PRE SATB-C-                                       
056900     05  FILLER              PIC X.                                       
057000*01    -COPY W0008     -PRE SATB-D-                                       
057100     05  FILLER              PIC X.                                       
057200     EJECT                                                                
057300*01    -COPY W0008     -PRE BENA-A-                                       
057400     05  FILLER              PIC X.                                       
057500     EJECT                                                                
057600*01    -COPY W0008     -PRE BENA-B-                                       
057700     05  FILLER              PIC X.                                       
057800     EJECT                                                                
057900*01    -COPY W0008     -PRE XXAZ-                                         
058000     05  FILLER              PIC X.                                       
058100     EJECT                                                                
058200 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDF5-PCB                       
058300                           ARTC-PCB SATB-PCB SATB-I-PCB                   
058400                           SATB-C-PCB SATB-D-PCB BENA-A-PCB               
058500                           BENA-B-PCB XXAZ-PCB.                           
058600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF5-PCB                      
058700                           ARTC-PCB SATB-PCB SATB-I-PCB                   
058800                           SATB-C-PCB SATB-D-PCB BENA-A-PCB               
058900                           BENA-B-PCB XXAZ-PCB.                           
059000                                                                          
059100     PERFORM IMS-GET-MSG                                                  
059200     IF SEGMENT-FINNS                                                     
059300       PERFORM A-INIT                                                     
059400       PERFORM B-KOLLA-NYCKLAR                                            
059500       IF NYCKLAR-OK                                                      
059600         MOVE NEJ TO ALLT-SW                                              
059700         PERFORM C-KOLLA-STRUKTURNR-TYP                                   
059800         IF (MFS-UPDATE)  AND (NOT STRUKTURNR-SPAERRAT)                   
059900                           AND (NOT STRUKTURNR-BORTTAGET)                 
060000           PERFORM D-KOLLA-UPDATE-TYP                                     
060100           PERFORM E-KOLLA-INPUT                                          
060200           IF INDATA-OK                                                   
060300             PERFORM F-UPPDATERA                                          
060400             MOVE JA TO ALLT-SW                                           
060500           END-IF                                                         
060600         ELSE                                                             
060700           IF MFS-FIRST                                                   
060800             MOVE JA TO ALLT-SW                                           
060900           ELSE                                                           
061000             PERFORM H-SAMMA-SIDA                                         
061100           END-IF                                                         
061200         END-IF                                                           
061300         IF ALLT-OK                                                       
061400           PERFORM G-LAES-VISA-STRUKTUR                                   
061500         END-IF                                                           
061600       END-IF                                                             
061700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
061800       PERFORM IMS-INSERT-MSG                                             
061900     END-IF                                                               
062000                                                                          
062100     MOVE ZERO TO RETURN-CODE                                             
062200     GOBACK                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 A-INIT SECTION.                                                          
062600                                                                          
062700     IF MSG-DUBBLA-TRANSKODER                                             
062800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I21101                 
062900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
063000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
063100     ELSE                                                                 
063200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I21101                  
063300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
063400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
063500     END-IF                                                               
063600                                                                          
063700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
063800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
063900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
064000                                                                          
064100     MOVE LOW-VALUE  TO MSG-AREA                                          
064200     MOVE 'W1O211N1' TO MFS-IDMOD                                         
064300     MOVE '1211'     TO MOD-IDTRANS                                       
064400     MOVE SPACE      TO MOD-TEMFSFEL MOD-TEMFSINF                         
064500                                                                          
064600     IF NOT EGEN-MID                                                      
064700       MOVE SPACE TO MFS-KDTRTYP                                          
064800       MOVE '7' TO MFS-IDPFK                                              
064900     END-IF                                                               
065000                                                                          
065100     ACCEPT DAGENS-DATUM FROM DATE                                        
065200     MOVE SAKNAS TO STRUKTURNR-TYP                                        
065300     MOVE SPACE  TO STRUKTURNR-PAA-ARTREG                                 
065400                                                                          
065500     MOVE +1 TO INDX                                                      
065600     PERFORM UNTIL INDX > MAX-TABELL-LAENGD                               
065700       MOVE +0 TO SATSNR(INDX)                                            
065800       ADD  +1 TO INDX                                                    
065900     END-PERFORM                                                          
066000                                                                          
066100     .                                                                    
066200     EJECT                                                                
066300 B-KOLLA-NYCKLAR SECTION.                                                 
066400                                                                          
066500     MOVE JA TO NYCKLAR-SW                                                
066600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
066700                             MOD-IDSKYLT-IN                               
066800                                                                          
066900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
067000     MOVE '001'             TO MSGI-KDCALL                                
067100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
067200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
067300     MOVE '1211'            TO MSGI-IDTRANS                               
067400     IF MFS-IDTRANS = '1211'                                              
067500     OR (MID-IDARTNR-IN NUMERIC                                           
067600     AND MID-IDARTNR-IN > ZERO)                                           
067700         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
067800     END-IF                                                               
067900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
068000     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
068100     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
068200                                                                          
068300     IF MSGI-IDLAND-SPR = 'GB'                                            
068400       MOVE +2    TO SPRAK-IX                                             
068500       MOVE 'GB ' TO MED-IDSKYLT                                          
068600     ELSE                                                                 
068700       MOVE +1    TO SPRAK-IX                                             
068800       MOVE 'S  ' TO MED-IDSKYLT                                          
068900     END-IF                                                               
069000                                                                          
069100     IF MID-IDARTNR-IN = ALL '+'                                          
069200       CONTINUE                                                           
069300     ELSE                                                                 
069400       MOVE '7'             TO MFS-IDPFK                                  
069500       MOVE SPACE           TO MFS-KDTRTYP                                
069600     END-IF                                                               
069700                                                                          
069800     IF (IDARTNR-WS NUMERIC) AND (IDARTNR-WS > ZERO) AND                  
069900          (IDARTNR-WS < 10000000)                                         
070000       MOVE IDARTNR-WS TO W-IDARTNR                                       
070100     ELSE                                                                 
070200       MOVE NEJ             TO NYCKLAR-SW                                 
070300       MOVE FEL4(SPRAK-IX)  TO MOD-TEMFSFEL                               
070400     END-IF                                                               
070500                                                                          
070600     IF MID-MED-IDSKYLT                                                   
070700       IF MID-IDSKYLT-IN = ALL '+'                                        
070800         IF MID-IDSKYLT-UT = SPACE                                        
070900           IF MSGI-IDLAND-SPR = 'GB'                                      
071000             MOVE 'GB ' TO IDSKYLT-WS                                     
071100           ELSE                                                           
071200             MOVE 'S  ' TO IDSKYLT-WS                                     
071300           END-IF                                                         
071400         ELSE                                                             
071500           MOVE MID-IDSKYLT-UT TO IDSKYLT-WS                              
071600         END-IF                                                           
071700       ELSE                                                               
071800         MOVE MID-IDSKYLT-IN TO IDSKYLT-WS                                
071900         MOVE '7'          TO MFS-IDPFK                                   
072000         MOVE SPACE        TO MFS-KDTRTYP                                 
072100       END-IF                                                             
072200     ELSE                                                                 
072300       IF MSGI-IDLAND-SPR = 'GB'                                          
072400         MOVE 'GB ' TO IDSKYLT-WS                                         
072500       ELSE                                                               
072600         MOVE 'S  ' TO IDSKYLT-WS                                         
072700       END-IF                                                             
072800     END-IF                                                               
072900                                                                          
073000     SET WWLAND03-IX TO +1                                                
073100     SEARCH WWLAND03-IDSKYLT-RAD                                          
073200       AT END                                                             
073300         MOVE NEJ               TO NYCKLAR-SW                             
073400         MOVE FEL4(SPRAK-IX)    TO MOD-TEMFSFEL                           
073500       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = IDSKYLT-WS                    
073600         CONTINUE                                                         
073700     END-SEARCH                                                           
073800                                                                          
073900     IF GODK-MID OR NYCKLAR-OK                                            
074000       MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                  
074100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
074200       MOVE IDSKYLT-WS TO MOD-IDSKYLT-UT                                  
074300     ELSE                                                                 
074400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
074500                               MOD-IDSKYLT-UT                             
074600       MOVE FEL7(SPRAK-IX)  TO MOD-TEMFSFEL                               
074700     END-IF                                                               
074800                                                                          
074900     IF NYCKLAR-FEL                                                       
075000       PERFORM MFS-RENSA-FAELT-IN                                         
075100       PERFORM MFS-RENSA-FAELT-UT                                         
075200       PERFORM MFS-RENSA-FAELT-BADE-IN-O-UT                               
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600 C-KOLLA-STRUKTURNR-TYP SECTION.                                          
075700******************************************                                
075800*  KOLLAR STRUKTURNR-TYP PÅ RASA         *                                
075900******************************************                                
076000                                                                          
076100     MOVE SAKNAS TO STRUKTURNR-TYP                                        
076200     MOVE NEJ      TO FINNS-OKONVERTERAT-SW                               
076300                      FINNS-KONVERTERAT-SW                                
076400                                                                          
076500     MOVE IDARTNR-WS TO W-IDARTNR                                         
076600     PERFORM IMS-GET-SATB01                                               
076700     IF SEGMENT-FINNS                                                     
076800       MOVE FINNS                TO STRUKTURNR-TYP                        
076900       MOVE JA                   TO FINNS-OKONVERTERAT-SW                 
077000       MOVE SATB01-STR-BEART-SVE TO BEART-RASA-SPAR                       
077100       MOVE SATB01-STR-KDBENHOM  TO KDBENHOM-RASA-SPAR                    
077200       IF SATB01-STR-TIBORT > 0                                           
077300         MOVE MED8(SPRAK-IX) TO MOD-TEMFSINF                              
077400         MOVE BORTTAGET TO STRUKTURNR-TYP                                 
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800******* KOLLAR OM STRUKTURNUMRET ÄR 'UNDER BEHANDLING'                    
077900                                                                          
078000     MOVE IDARTNR-WS TO IDARTNR2-WS                                       
078100     COMPUTE IDARTNR-KONVERTERAT-WS = 999999999 - IDARTNR2-WS             
078200     MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                             
078300     PERFORM IMS-GET-SATB01                                               
078400     IF SEGMENT-FINNS                                                     
078500                                                                          
078600       MOVE 001                 TO WORK-KDCALL                            
078700       MOVE WC-CDC-SE           TO WORK-IDDC                              
078800       MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
078900       MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                      
079000       CALL WORKDAY USING WORK-KDCALL                                     
079100                          WORK-DATE-AREA                                  
079200                          WORK-KDSVAR                                     
079300       IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
079400*********** OM MAN TRÄFFAR PÅ EN KONVERTERAD                              
079500*********** STRUKTUR SOM ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                
079600*********** ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.               
079700         PERFORM IMS-DLET-SATB                                            
079800       ELSE                                                               
079900         IF MSG-SIGNON-USERID = SATB01-STR-IDUSER                         
080000           MOVE FINNS                TO STRUKTURNR-TYP                    
080100           MOVE JA                   TO FINNS-KONVERTERAT-SW              
080200           MOVE SATB01-STR-BEART-SVE TO BEART-RASA-SPAR                   
080300           MOVE SATB01-STR-KDBENHOM  TO KDBENHOM-RASA-SPAR                
080400           PERFORM S08-KOLLA-OM-USER-HAR-LAASNING                         
080500           IF INDATA-OK                                                   
080600             CONTINUE                                                     
080700           ELSE                                                           
080800             MOVE MED7(SPRAK-IX)  TO MOD-TEMFSINF                         
080900             MOVE SPAERRAT TO STRUKTURNR-TYP                              
081000           END-IF                                                         
081100         ELSE                                                             
081200*********  STRUKTUREN KONVERTERAD VIA BILD 1221                           
081300*********  UPPDATERING EJ TILLÅTEN                                        
081400           MOVE MED7(SPRAK-IX)    TO MOD-TEMFSINF                         
081500           MOVE SPAERRAT          TO STRUKTURNR-TYP                       
081600         END-IF                                                           
081700       END-IF                                                             
081800      END-IF                                                              
081900                                                                          
082000      IF (NOT MFS-UPDATE) AND (STRUKTURNR-SAKNAS)                         
082100        MOVE FEL1(SPRAK-IX)       TO MOD-TEMFSFEL                         
082200      END-IF                                                              
082300                                                                          
082400      MOVE IDARTNR-WS TO W-IDARTNR                                        
082500      PERFORM IMS-GET-ARTC01                                              
082600      IF SEGMENT-FINNS                                                    
082700        MOVE FINNS TO STRUKTURNR-PAA-ARTREG                               
082800      END-IF                                                              
082900      .                                                                   
083000      EJECT                                                               
083100 D-KOLLA-UPDATE-TYP SECTION.                                              
083200******************************************                                
083300*  KOLLAR TYP AV UPPDATERING VID PF11    *                                
083400******************************************                                
083500                                                                          
083600     IF MID-BORT = SPACE                                                  
083700****** BEHANDLAS SOM EJ IFYLLD                                            
083800       MOVE '+' TO MID-BORT                                               
083900     END-IF                                                               
084000                                                                          
084100     IF STRUKTURNR-FINNS                                                  
084200       IF (MID-BORT NOT = ALL '+')                                        
084300         MOVE BORTTAG TO UPDATE-TYP                                       
084400       ELSE                                                               
084500         IF (MID-IDARTNR-NY NOT = ALL '+')                                
084600           MOVE NYUPPLAEGG-KOPIERING TO UPDATE-TYP                        
084700         ELSE                                                             
084800           MOVE AENDRING TO UPDATE-TYP                                    
084900         END-IF                                                           
085000       END-IF                                                             
085100     ELSE                                                                 
085200       MOVE NYUPPLAEGG TO UPDATE-TYP                                      
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600 E-KOLLA-INPUT SECTION.                                                   
085700                                                                          
085800     MOVE JA  TO INDATA-SW                                                
085900                                                                          
086000     IF IDSKYLT-WS = 'S  '                                                
086100       CONTINUE                                                           
086200     ELSE                                                                 
086300       MOVE 'GB' TO IDSKYLT-WS                                            
086400     END-IF                                                               
086500                                                                          
086600     IF MID-BEART-IN = SPACE                                              
086700****** BEHANDLAS SOM EJ IFYLLD                                            
086800       MOVE '+++++++++++++++++++++++++' TO MID-BEART-IN                   
086900     END-IF                                                               
087000                                                                          
087100     IF MID-IDSTRTYP-IN = SPACE                                           
087200****** BEHANDLAS SOM EJ IFYLLD                                            
087300       MOVE '+' TO MID-IDSTRTYP-IN                                        
087400     END-IF                                                               
087500                                                                          
087600     IF MID-INPUT = ALL '+'                                               
087700       MOVE NEJ TO INDATA-SW                                              
087800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
087900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
088000       PERFORM MFS-ROER-EJ-FAELT-BADE-IN-O-UT                             
088100                                                                          
088200       IF (UPDATE-NYUPPL) AND (STRUKTURNR-FINNS-PAA-ARTREG)               
088300*******  VISA ATT STRUKTURTYP MÅSTE FYLLAS I                              
088400         MOVE FEL2(SPRAK-IX)     TO MOD-TEMFSFEL                          
088500         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
088600       ELSE                                                               
088700         MOVE MED1(SPRAK-IX) TO MOD-TEMFSINF                              
088800       END-IF                                                             
088900       PERFORM S06-STAENG-EVENTUELLA-FAELT                                
089000     ELSE                                                                 
089100       IF UPDATE-BORTTAG                                                  
089200         PERFORM EA-KOLLA-BORTTAG-INPUT                                   
089300       ELSE                                                               
089400         IF UPDATE-AENDRA                                                 
089500           PERFORM EB-KOLLA-AENDRING-INPUT                                
089600         ELSE                                                             
089700           IF UPDATE-NYUPPL                                               
089800             PERFORM EC-KOLLA-NYUPPLAEGG-INPUT                            
089900           ELSE                                                           
090000             PERFORM ED-KOLLA-KOPIERING-INPUT                             
090100           END-IF                                                         
090200         END-IF                                                           
090300       END-IF                                                             
090400                                                                          
090500       IF INDATA-FEL                                                      
090600         IF MOD-TEMFSFEL = SPACE                                          
090700********** TEMFSFEL FYLLS I OM EJ REDAN IFYLLD                            
090800           MOVE FEL2(SPRAK-IX) TO MOD-TEMFSFEL                            
090900         END-IF                                                           
091000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
091100         PERFORM MFS-ROER-EJ-FAELT-UT                                     
091200         PERFORM MFS-ROER-EJ-FAELT-BADE-IN-O-UT                           
091300                                                                          
091400         PERFORM S06-STAENG-EVENTUELLA-FAELT                              
091500                                                                          
091600       END-IF                                                             
091700     END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000 EA-KOLLA-BORTTAG-INPUT SECTION.                                          
092100                                                                          
092200     IF MID-BORT = 'J' OR 'Y'                                             
092300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORT-ATTR                         
092400                                                                          
092500******************************************                                
092600*  KOLLAR ATT INGET ANNAT FÄLT ÄR IFYLLT *                                
092700******************************************                                
092800                                                                          
092900       IF (MID-IDARTNR-NY = ALL '+' OR SPACE)                             
093000         CONTINUE                                                         
093100       ELSE                                                               
093200         MOVE NEJ                TO INDATA-SW                             
093300         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                   
093400       END-IF                                                             
093500                                                                          
093600       IF (MID-BEART-IN = ALL '+' OR SPACE)                               
093700         CONTINUE                                                         
093800       ELSE                                                               
093900         MOVE NEJ                TO INDATA-SW                             
094000         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                     
094100       END-IF                                                             
094200                                                                          
094300       IF (MID-KDBENHOM-IN = ALL '+')                                     
094400         CONTINUE                                                         
094500       ELSE                                                               
094600         MOVE NEJ               TO INDATA-SW                              
094700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                   
094800       END-IF                                                             
094900                                                                          
095000       IF (MID-KDPRODSL-IN = ALL '+')                                     
095100         CONTINUE                                                         
095200       ELSE                                                               
095300         MOVE NEJ               TO INDATA-SW                              
095400         MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                   
095500       END-IF                                                             
095600                                                                          
095700       IF (MID-IDFKNGRP-IN = ALL '+')                                     
095800         CONTINUE                                                         
095900       ELSE                                                               
096000         MOVE NEJ               TO INDATA-SW                              
096100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                   
096200       END-IF                                                             
096300                                                                          
096400       IF (MID-IDSTRTYP-IN = ALL '+')                                     
096500         CONTINUE                                                         
096600       ELSE                                                               
096700         MOVE NEJ                TO INDATA-SW                             
096800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
096900       END-IF                                                             
097000                                                                          
097100       IF (MID-TESTRNOT(1) = ALL '+')                                     
097200         CONTINUE                                                         
097300       ELSE                                                               
097400         MOVE NEJ                TO INDATA-SW                             
097500         MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(2)                  
097600       END-IF                                                             
097700                                                                          
097800       IF (MID-TESTRNOT(2) = ALL '+')                                     
097900         CONTINUE                                                         
098000       ELSE                                                               
098100         MOVE NEJ                TO INDATA-SW                             
098200         MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(2)                  
098300       END-IF                                                             
098400                                                                          
098500       IF INDATA-OK                                                       
098600         IF (STRUKTURNR-FINNS-KONVERTERAT) AND                            
098700            (NOT STRUKTURNR-FINNS-OKONVERTERAT)                           
098800********   OM STRUKTUREN BARA FINNS I KONVERTERAD FORM                    
098900           CONTINUE                                                       
099000         ELSE                                                             
099100           IF (STRUKTURNR-FINNS-KONVERTERAT) AND                          
099200              (STRUKTURNR-FINNS-OKONVERTERAT)                             
099300********     OM STRUKTUREN FINNS I BÅDE KONVERTERAD FORM                  
099400********     OCH I 'RIKTIG' FORM                                          
099500             MOVE NEJ                TO INDATA-SW                         
099600             MOVE MED3(SPRAK-IX)     TO MOD-TEMFSINF                      
099700             MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                     
099800           ELSE                                                           
099900             IF (STRUKTURNR-FINNS-OKONVERTERAT)                           
100000*************  OM STRUKTUREN BARA FINNS I 'RIKTIG' FORM                   
100100                                                                          
100200                                                                          
100300               IF STRUKTURNR-FINNS-PAA-ARTREG                             
100400***************  BORTTAG KAN VARA TILLÅTEN OM STRUKTURTYP = 'S'           
100500***************  OCH SORT PÅ ARTREG EJ ÄR 'SA'                            
100600                                                                          
100700                 MOVE IDARTNR-WS TO W-IDARTNR                             
100800                 PERFORM IMS-GET-SATB01                                   
100900                 IF SATB01-STR-IDSTRTYP = 'S'                             
101000                   PERFORM IMS-GET-ARTC01                                 
101100                   IF ART-KDSORT = 'SA' OR 'TM'                           
101200                     MOVE NEJ                TO INDATA-SW                 
101300                     MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR             
101400                   ELSE                                                   
101500                     PERFORM EAA-KOLLA-ATT-EJ-RADER                       
101600                     IF INDATA-OK                                         
101700                       PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS              
101800                       IF INDATA-OK                                       
101900                         CONTINUE                                         
102000                       ELSE                                               
102100                         MOVE MED13(SPRAK-IX)    TO MOD-TEMFSINF          
102200                         MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR         
102300                       END-IF                                             
102400                     ELSE                                                 
102500                       MOVE MED16(SPRAK-IX)    TO MOD-TEMFSINF            
102600                       MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR           
102700                     END-IF                                               
102800                   END-IF                                                 
102900                 ELSE                                                     
103000                   MOVE NEJ                TO INDATA-SW                   
103100                   MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR               
103200                 END-IF                                                   
103300               ELSE                                                       
103400                 PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                    
103500                 IF INDATA-OK                                             
103600                   CONTINUE                                               
103700                 ELSE                                                     
103800                   MOVE MED13(SPRAK-IX)    TO MOD-TEMFSINF                
103900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR               
104000                 END-IF                                                   
104100               END-IF                                                     
104200             ELSE                                                         
104300*************  OM STRUKTUREN SAKNAS                                       
104400               MOVE NEJ                TO INDATA-SW                       
104500               MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                   
104600             END-IF                                                       
104700           END-IF                                                         
104800         END-IF                                                           
104900       END-IF                                                             
105000                                                                          
105100     ELSE                                                                 
105200       PERFORM MFS-LAES-IN-IGEN                                           
105300       MOVE NEJ                TO INDATA-SW                               
105400       MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                           
105500     END-IF                                                               
105600     .                                                                    
105700     SKIP3                                                                
105800 EAA-KOLLA-ATT-EJ-RADER SECTION.                                          
105900******************************************                                
106000* KONTROLL ATT INGA GÄLLANDE RADER FINNS *                                
106100******************************************                                
106200                                                                          
106300     PERFORM IMS-GET-SATB11                                               
106400     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
106500       IF SEGMENT-FINNS                                                   
106600         MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                        
106700         MOVE DAGENS-DATUM          TO TMP2-YYMMDD                        
106800         PERFORM WY2000P1                                                 
106900         IF TMP1-YYMMDD <= TMP2-YYMMDD                                    
107000           PERFORM IMS-GET-SATB11                                         
107100         ELSE                                                             
107200           MOVE NEJ TO INDATA-SW                                          
107300         END-IF                                                           
107400       END-IF                                                             
107500     END-PERFORM                                                          
107600     .                                                                    
107700     EJECT                                                                
107800 EB-KOLLA-AENDRING-INPUT SECTION.                                         
107900                                                                          
108000     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
108100       PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                             
108200     ELSE                                                                 
108300       PERFORM EBA-KOLLA-SAKN-PA-ARTREG-INPU                              
108400     END-IF                                                               
108500                                                                          
108600     IF (MID-TESTRNOT(1) = ALL '+')                                       
108700       CONTINUE                                                           
108800     ELSE                                                                 
108900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(1)                  
109000     END-IF                                                               
109100                                                                          
109200     IF (MID-TESTRNOT(2) = ALL '+')                                       
109300       CONTINUE                                                           
109400     ELSE                                                                 
109500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(2)                  
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900 EBA-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
110000*************************************************                         
110100* *KOLL VID ÄNDRING OCH IDARTNR SAKNAS PÅ ARTREG*                         
110200*************************************************                         
110300                                                                          
110400     PERFORM S03-KOLLA-BEART-KDHOM-AENDRING                               
110500                                                                          
110600     IF (MID-KDPRODSL-IN = ALL '+')                                       
110700       CONTINUE                                                           
110800     ELSE                                                                 
110900       IF MID-KDPRODSL-IN NUMERIC                                         
111000         MOVE MID-KDPRODSL-IN TO KDPRODSL-GODK-WS                         
111100         IF KDPRODSL-GODK                                                 
111200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-IN-ATTR               
111300         ELSE                                                             
111400           MOVE NEJ               TO INDATA-SW                            
111500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                 
111600         END-IF                                                           
111700       ELSE                                                               
111800         MOVE NEJ               TO INDATA-SW                              
111900         MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                   
112000       END-IF                                                             
112100     END-IF                                                               
112200                                                                          
112300     IF (MID-IDFKNGRP-IN = ALL '+')                                       
112400       CONTINUE                                                           
112500     ELSE                                                                 
112600       IF MID-IDFKNGRP-IN NUMERIC                                         
112700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-IN-ATTR                 
112800       ELSE                                                               
112900         MOVE NEJ               TO INDATA-SW                              
113000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                   
113100       END-IF                                                             
113200     END-IF                                                               
113300                                                                          
113400     IF (MID-IDSTRTYP-IN = ALL '+')                                       
113500       CONTINUE                                                           
113600     ELSE                                                                 
113700       IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                           
113800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR                
113900       ELSE                                                               
114000         MOVE NEJ                TO INDATA-SW                             
114100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
114200       END-IF                                                             
114300     END-IF                                                               
114400     .                                                                    
114500     EJECT                                                                
114600 EC-KOLLA-NYUPPLAEGG-INPUT SECTION.                                       
114700                                                                          
114800     MOVE IDARTNR-WS TO IDARTNR-NYUPPL-WS                                 
114900                                                                          
115000     IF MID-IDARTNR-NY = ALL '+'                                          
115100       CONTINUE                                                           
115200     ELSE                                                                 
115300       MOVE NEJ               TO INDATA-SW                                
115400       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                      
115500     END-IF                                                               
115600                                                                          
115700     IF INDATA-OK                                                         
115800       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
115900       PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                              
116000       IF INDATA-OK                                                       
116100                                                                          
116200         PERFORM IMS-GET-ARTC01                                           
116300         IF SEGMENT-FINNS                                                 
116400           IF ART-KDERS-UTG > 0                                           
116500             MOVE NEJ               TO INDATA-SW                          
116600             MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                       
116700             MOVE MED15(SPRAK-IX)   TO MOD-TEMFSINF                       
116800           ELSE                                                           
116900             PERFORM IMS-GET-ARTC11                                       
117000             IF CLAG-KDERS > 0                                            
117100               MOVE NEJ              TO INDATA-SW                         
117200               MOVE FEL5(SPRAK-IX)   TO MOD-TEMFSFEL                      
117300               MOVE MED11(SPRAK-IX)  TO MOD-TEMFSINF                      
117400             ELSE                                                         
117500               PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                     
117600             END-IF                                                       
117700           END-IF                                                         
117800                                                                          
117900         ELSE                                                             
118000           PERFORM ECA-KOLLA-SAKN-PA-ARTREG-INPU                          
118100         END-IF                                                           
118200                                                                          
118300         IF (MID-TESTRNOT(1) = ALL '+')                                   
118400           CONTINUE                                                       
118500         ELSE                                                             
118600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(1)              
118700         END-IF                                                           
118800                                                                          
118900         IF (MID-TESTRNOT(2) = ALL '+')                                   
119000           CONTINUE                                                       
119100         ELSE                                                             
119200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(2)              
119300         END-IF                                                           
119400                                                                          
119500       ELSE                                                               
119600         MOVE NEJ               TO INDATA-SW                              
119700         MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                           
119800         MOVE MED13(SPRAK-IX)   TO MOD-TEMFSINF                           
119900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                    
120000       END-IF                                                             
120100                                                                          
120200     ELSE                                                                 
120300       PERFORM MFS-LAES-IN-IGEN                                           
120400     END-IF                                                               
120500     .                                                                    
120600     EJECT                                                                
120700 ECA-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
120800*************************************************                         
120900* KOLL VID NYUPPL OCH IDARTNR SAKNAS PÅ ARTREG  *                         
121000*************************************************                         
121100                                                                          
121200     PERFORM S04-KOLLA-BEART-KDHOM-NYUPPL                                 
121300                                                                          
121400     IF (MID-KDPRODSL-IN = ALL '+')                                       
121500       MOVE NEJ               TO INDATA-SW                                
121600       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                     
121700     ELSE                                                                 
121800       IF MID-KDPRODSL-IN NUMERIC                                         
121900         MOVE MID-KDPRODSL-IN TO KDPRODSL-GODK-WS                         
122000         IF KDPRODSL-GODK                                                 
122100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-IN-ATTR               
122200         ELSE                                                             
122300           MOVE NEJ               TO INDATA-SW                            
122400           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                 
122500         END-IF                                                           
122600       ELSE                                                               
122700         MOVE NEJ               TO INDATA-SW                              
122800         MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                   
122900       END-IF                                                             
123000     END-IF                                                               
123100                                                                          
123200     IF (MID-IDFKNGRP-IN = ALL '+')                                       
123300       MOVE NEJ               TO INDATA-SW                                
123400       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                     
123500     ELSE                                                                 
123600       IF MID-IDFKNGRP-IN NUMERIC                                         
123700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-IN-ATTR                 
123800       ELSE                                                               
123900         MOVE NEJ               TO INDATA-SW                              
124000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                   
124100       END-IF                                                             
124200     END-IF                                                               
124300                                                                          
124400       IF (MID-IDSTRTYP-IN = ALL '+')                                     
124500         MOVE NEJ                TO INDATA-SW                             
124600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
124700       ELSE                                                               
124800         IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                         
124900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
125000         ELSE                                                             
125100           MOVE NEJ                TO INDATA-SW                           
125200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
125300         END-IF                                                           
125400       END-IF                                                             
125500     .                                                                    
125600     EJECT                                                                
125700 ED-KOLLA-KOPIERING-INPUT SECTION.                                        
125800                                                                          
125900     MOVE SAKNAS TO STRUKTURNR-NY-TYP                                     
126000                                                                          
126100     MOVE MID-IDARTNR-NY TO IDARTNR2-WS                                   
126200     INSPECT IDARTNR2-WS REPLACING LEADING SPACE BY ZERO                  
126300     IF IDARTNR2-WS NUMERIC                                               
126400       IF (IDARTNR2-WS > 0 AND < 10000000)                                
126500         PERFORM EDA-KOLL-OM-KOPIERING-TILLATEN                           
126600         IF INDATA-OK                                                     
126700           MOVE IDARTNR2-WS TO IDARTNR-NYUPPL-WS                          
126800           COMPUTE IDARTNR-NYUPPL-KONVERTERAT-WS = 999999999 -            
126900                                               IDARTNR-NYUPPL-WS          
127000                                                                          
127100           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
127200           PERFORM IMS-GET-SATB01                                         
127300           IF SEGMENT-FINNS                                               
127400             MOVE FINNS TO STRUKTURNR-NY-TYP                              
127500             PERFORM IMS-GET-SATB11                                       
127600             IF SEGMENT-FINNS                                             
127700*************  OM DET NYA STRUKTURNUMRET REDAN FINNS PÅ RASA              
127800*************  FÅR DET EJ HA NÅGRA RADER REGISTRERADE                     
127900               MOVE NEJ               TO INDATA-SW                        
128000               MOVE FEL2(SPRAK-IX)    TO MOD-TEMFSFEL                     
128100               MOVE MED4(SPRAK-IX)    TO MOD-TEMFSINF                     
128200               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR              
128300             END-IF                                                       
128400           END-IF                                                         
128500                                                                          
128600           IF INDATA-OK                                                   
128700             MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO W-IDARTNR              
128800             PERFORM IMS-GET-SATB01                                       
128900             IF SEGMENT-FINNS                                             
129000*********** DET NYA STRUKTURNUMRET FÅR INTE REDAN FINNAS PÅ RASA          
129100*********** I 'KONVERTERAD FORM'                                          
129200               MOVE NEJ               TO INDATA-SW                        
129300               MOVE FEL2(SPRAK-IX)    TO MOD-TEMFSFEL                     
129400               MOVE MED10(SPRAK-IX)   TO MOD-TEMFSINF                     
129500               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR              
129600             ELSE                                                         
129700               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-NY-ATTR            
129800                                                                          
129900               IF STRUKTURNR-FINNS-KONVERTERAT                            
130000*********** DET KOPIERADE STRUKTURNUMRET FÅR INTE FINNAS PÅ RASA          
130100*********** I 'KONVERTERAD FORM' UTAN ENDAST I 'RIKTIG' FORM              
130200                 MOVE NEJ              TO INDATA-SW                       
130300                 MOVE FEL5(SPRAK-IX)   TO MOD-TEMFSFEL                    
130400                 MOVE MED12(SPRAK-IX)  TO MOD-TEMFSINF                    
130500               END-IF                                                     
130600             END-IF                                                       
130700           END-IF                                                         
130800         ELSE                                                             
130900           MOVE FEL6(SPRAK-IX)  TO MOD-TEMFSFEL                           
131000           MOVE MED14(SPRAK-IX) TO MOD-TEMFSINF                           
131100         END-IF                                                           
131200       ELSE                                                               
131300         MOVE NEJ TO INDATA-SW                                            
131400       END-IF                                                             
131500     ELSE                                                                 
131600       MOVE NEJ             TO INDATA-SW                                  
131700       MOVE FEL3(SPRAK-IX)  TO MOD-TEMFSINF                               
131800     END-IF                                                               
131900                                                                          
132000     IF INDATA-OK                                                         
132100       IF STRUKTURNR-NY-FINNS                                             
132200*******  NYTT IDARTNR-NYUPPL FINNS REDAN SOM ROT PÅ RASA                  
132300*******  LYS UPP EV. FÖRSÖK TILL ÄNDRING AV ARTIKELUPPG                   
132400         PERFORM S09-LYS-UPP-INMATADE-FAELT                               
132500         IF INDATA-FEL                                                    
132600           MOVE FEL2(SPRAK-IX)   TO MOD-TEMFSFEL                          
132700           MOVE MED17(SPRAK-IX)  TO MOD-TEMFSINF                          
132800         END-IF                                                           
132900       END-IF                                                             
133000                                                                          
133100       IF INDATA-OK                                                       
133200         MOVE NEJ TO INGAAR-I-KOPIERAD-SATS-SW                            
133300         PERFORM EDB-KOLLA-OM-INGAAR-I-KOP-SATS                           
133400                                                                          
133500         IF INGAAR-EJ-I-KOPIERAD-SATS                                     
133600           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
133700           PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                          
133800           IF INDATA-OK                                                   
133900                                                                          
134000             MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                          
134100             PERFORM IMS-GET-ARTC01                                       
134200             IF SEGMENT-FINNS                                             
134300               MOVE FINNS TO STRUKTURNR-NY-PAA-ARTREG                     
134400                                                                          
134500               IF ART-KDERS-UTG > 0                                       
134600                 MOVE NEJ                  TO INDATA-SW                   
134700                 MOVE FEL5(SPRAK-IX)       TO MOD-TEMFSFEL                
134800                 MOVE MED15(SPRAK-IX)      TO MOD-TEMFSINF                
134900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR            
135000               ELSE                                                       
135100                 PERFORM IMS-GET-ARTC11                                   
135200                 IF CLAG-KDERS > 0                                        
135300                   MOVE NEJ                TO INDATA-SW                   
135400                   MOVE FEL5(SPRAK-IX)     TO MOD-TEMFSFEL                
135500                   MOVE MED11(SPRAK-IX)    TO MOD-TEMFSINF                
135600                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR          
135700                 ELSE                                                     
135800                   PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                 
135900                 END-IF                                                   
136000               END-IF                                                     
136100                                                                          
136200             ELSE                                                         
136300               PERFORM EDC-KOLLA-SAKN-PA-ARTREG-INPU                      
136400             END-IF                                                       
136500                                                                          
136600             IF (MID-TESTRNOT(1) = ALL '+')                               
136700               CONTINUE                                                   
136800             ELSE                                                         
136900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(1)          
137000             END-IF                                                       
137100                                                                          
137200             IF (MID-TESTRNOT(2) = ALL '+')                               
137300               CONTINUE                                                   
137400             ELSE                                                         
137500               MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESTRNOT-ATTR(2)          
137600             END-IF                                                       
137700                                                                          
137800           ELSE                                                           
137900             MOVE NEJ               TO INDATA-SW                          
138000             MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                       
138100             MOVE MED13(SPRAK-IX)   TO MOD-TEMFSINF                       
138200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                
138300           END-IF                                                         
138400                                                                          
138500         ELSE                                                             
138600           MOVE NEJ               TO INDATA-SW                            
138700           MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                         
138800           MOVE MED9(SPRAK-IX)    TO MOD-TEMFSINF                         
138900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                  
139000         END-IF                                                           
139100       END-IF                                                             
139200                                                                          
139300     ELSE                                                                 
139400       PERFORM MFS-LAES-IN-IGEN                                           
139500       IF NOT STRUKTURNR-FINNS-KONVERTERAT                                
139600*******  OM INDATA-FEL EJ SATT PGA DETTA                                  
139700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NY-ATTR                    
139800       END-IF                                                             
139900     END-IF                                                               
140000     .                                                                    
140100     EJECT                                                                
140200 EDA-KOLL-OM-KOPIERING-TILLATEN SECTION.                                  
140300*****************************************************                     
140400* KOPIERING ENDAST TILLÅTEN OM 'SIGNON-USERID':     *                     
140500* ¤ INTE HAR NÅGON GÄLLANDE LÅSNING PÅ WDR5 OCH     *                     
140600*   INTE HAR NÅGRA GÄLLANDE KONVERTERADE STRUKTURER.*                     
140700*****************************************************                     
140800                                                                          
140900     PERFORM S08-KOLLA-OM-USER-HAR-LAASNING                               
141000                                                                          
141100     IF INDATA-OK                                                         
141200       PERFORM IMS-GET-SATB01-DSEQ                                        
141300       PERFORM UNTIL SEGMENT-SAKNAS                                       
141400         IF SEGMENT-FINNS                                                 
141500                                                                          
141600           MOVE 001                 TO WORK-KDCALL                        
141700           MOVE WC-CDC-SE           TO WORK-IDDC                          
141800           MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                  
141900           MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                  
142000           CALL WORKDAY USING WORK-KDCALL                                 
142100                              WORK-DATE-AREA                              
142200                              WORK-KDSVAR                                 
142300           IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                     
142400***********  OM MAN TRÄFFAR PÅ EN KOVERTERAD STRUKTUR                     
142500***********  SOM ÄR ÄLDRE ÄN 2 IGNORERAS DEN.                             
142600***********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.              
142700                                                                          
142800             PERFORM IMS-GET-SATB01-DSEQ                                  
142900           ELSE                                                           
143000             MOVE 'GE' TO STATUS-WS                                       
143100             MOVE NEJ  TO INDATA-SW                                       
143200           END-IF                                                         
143300         END-IF                                                           
143400       END-PERFORM                                                        
143500     END-IF                                                               
143600     .                                                                    
143700     EJECT                                                                
143800 EDB-KOLLA-OM-INGAAR-I-KOP-SATS SECTION.                                  
143900**********************************************************                
144000*   KONTROLL ATT DET NYA STRUKTURNUMRET EJ INGÅR SOM RAD *                
144100*   I KOPIERAD STRUKTUR ELLER I "SATS-I-SATS".VID PÅ-    *                
144200*   TRÄFFADE AV EN SATS ELLER KOMPLETTENHET LÄGGS DETTA  *                
144300*   ARTIKELNR I EN TABELL .MAN LÄSER SEDAN ARTIKELNUMREN *                
144400*   I TABELLEN OCH KOLLAR DESSA O.S.V.                   *                
144500**********************************************************                
144600                                                                          
144700     MOVE +1 TO INDX                                                      
144800                INDX2                                                     
144900     MOVE NEJ            TO SATSTABELL-SLUT-SW                            
145000     MOVE MID-IDARTNR-NY TO IDARTNR-NYUPPL-WS                             
145100     MOVE IDARTNR-WS     TO W-IDARTNR                                     
145200     PERFORM UNTIL SATSTABELL-SLUT                                        
145300                                                                          
145400       PERFORM IMS-GET-SATB01                                             
145500       IF SEGMENT-FINNS                                                   
145600         PERFORM IMS-GET-SATB11                                           
145700         IF SEGMENT-FINNS                                                 
145800           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
145900                           (INGAAR-I-KOPIERAD-SATS)                       
146000             MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                    
146100             MOVE DAGENS-DATUM          TO TMP2-YYMMDD                    
146200             PERFORM WY2000P1                                             
146300             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
146400               PERFORM IMS-GET-SATB11                                     
146500             ELSE                                                         
146600               IF SATB11-RAD-IDARTNR = IDARTNR-NYUPPL-WS                  
146700                 MOVE JA TO INGAAR-I-KOPIERAD-SATS-SW                     
146800                            SATSTABELL-SLUT-SW                            
146900               ELSE                                                       
147000                 IF SATB11-RAD-IDSTRTYP = 'S' OR 'K'                      
147100*****************  OM INGÅENDE RAD ÄR EN SATS                             
147200*****************  ELLER EN KOMPLETTENHET                                 
147300                   MOVE SATB11-RAD-IDARTNR TO SATSNR(INDX)                
147400                   ADD +1 TO INDX                                         
147500                 END-IF                                                   
147600                 PERFORM IMS-GET-SATB11                                   
147700               END-IF                                                     
147800             END-IF                                                       
147900           END-PERFORM                                                    
148000         END-IF                                                           
148100       END-IF                                                             
148200                                                                          
148300       IF SATSNR(INDX2) = ZERO                                            
148400         MOVE JA TO SATSTABELL-SLUT-SW                                    
148500       ELSE                                                               
148600         MOVE SATSNR(INDX2) TO W-IDARTNR                                  
148700         ADD +1             TO INDX2                                      
148800       END-IF                                                             
148900     END-PERFORM                                                          
149000     .                                                                    
149100     EJECT                                                                
149200 EDC-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
149300*********************************************************                 
149400* KOLL VID KOPIERING OCH IDARTNR(NYTT) SAKNAS PÅ ARTREG *                 
149500*********************************************************                 
149600                                                                          
149700     PERFORM S05-KOLL-BEART-KDHOM-KOPIERING                               
149800                                                                          
149900     IF (MID-KDPRODSL-IN = ALL '+')                                       
150000       CONTINUE                                                           
150100     ELSE                                                                 
150200       IF MID-KDPRODSL-IN NUMERIC                                         
150300         MOVE MID-KDPRODSL-IN TO KDPRODSL-GODK-WS                         
150400         IF KDPRODSL-GODK                                                 
150500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-IN-ATTR               
150600         ELSE                                                             
150700           MOVE NEJ               TO INDATA-SW                            
150800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                 
150900         END-IF                                                           
151000       ELSE                                                               
151100         MOVE NEJ               TO INDATA-SW                              
151200         MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                   
151300       END-IF                                                             
151400     END-IF                                                               
151500                                                                          
151600     IF (MID-IDFKNGRP-IN = ALL '+')                                       
151700       CONTINUE                                                           
151800     ELSE                                                                 
151900       IF MID-IDFKNGRP-IN NUMERIC                                         
152000         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-IN-ATTR                 
152100       ELSE                                                               
152200         MOVE NEJ               TO INDATA-SW                              
152300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                   
152400       END-IF                                                             
152500     END-IF                                                               
152600                                                                          
152700     IF (MID-IDSTRTYP-IN = ALL '+')                                       
152800       CONTINUE                                                           
152900     ELSE                                                                 
153000       IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                           
153100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR                
153200       ELSE                                                               
153300         MOVE NEJ                TO INDATA-SW                             
153400         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
153500       END-IF                                                             
153600     END-IF                                                               
153700     .                                                                    
153800     EJECT                                                                
153900 F-UPPDATERA SECTION.                                                     
154000                                                                          
154100     IF MID-INPUT NOT = ALL '+'                                           
154200       IF UPDATE-BORTTAG                                                  
154300         PERFORM FA-TABORT-SATSSTRUKTUR                                   
154400       ELSE                                                               
154500         IF UPDATE-AENDRA                                                 
154600           PERFORM FB-AENDRA-SATSSTRUKTUR                                 
154700         ELSE                                                             
154800           IF UPDATE-NYUPPL                                               
154900             PERFORM FC-NYUPPL-SATSSTRUKTUR                               
155000           ELSE                                                           
155100             PERFORM FD-KOPIERA-SATSSTRUKTUR                              
155200           END-IF                                                         
155300         END-IF                                                           
155400       END-IF                                                             
155500       PERFORM MFS-FORM-ATTR                                              
155600                                                                          
155700       IF UPDATE-NYUPPL-KOPIERING                                         
155800         IF STRUKTURNR-NY-FINNS                                           
155900           MOVE MED18(SPRAK-IX) TO MOD-TEMFSINF                           
156000         ELSE                                                             
156100           MOVE MED2(SPRAK-IX)  TO MOD-TEMFSINF                           
156200         END-IF                                                           
156300         INSPECT IDARTNR-NYUPPL-WS                                        
156400            REPLACING LEADING ZERO BY SPACE                               
156500         MOVE IDARTNR-NYUPPL-WS TO MOD-IDARTNR-UT                         
156600       ELSE                                                               
156700         MOVE MED5(SPRAK-IX)    TO MOD-TEMFSINF                           
156800       END-IF                                                             
156900     END-IF                                                               
157000                                                                          
157100     .                                                                    
157200     EJECT                                                                
157300 FA-TABORT-SATSSTRUKTUR SECTION.                                          
157400**************************************************                        
157500* BÅDE 'OKONVERTERAD' OCH EVENTUELL KONVERTERAD  *                        
157600* STRUKTUR TAS BORT                              *                        
157700**************************************************                        
157800                                                                          
157900     MOVE SAKNAS TO STRUKTURNR-TYP                                        
158000                                                                          
158100     MOVE IDARTNR-WS TO W-IDARTNR                                         
158200     PERFORM IMS-GET-SATB01                                               
158300     IF SEGMENT-FINNS                                                     
158400       PERFORM IMS-DLET-SATB                                              
158500     END-IF                                                               
158600                                                                          
158700     MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                             
158800     PERFORM IMS-GET-SATB01                                               
158900     IF SEGMENT-FINNS                                                     
159000       PERFORM IMS-DLET-SATB                                              
159100     END-IF                                                               
159200                                                                          
159300*    PERFORM MFS-RENSA-FAELT-UT                                           
159400*    PERFORM MFS-RENSA-FAELT-BADE-IN-O-UT                                 
159500     .                                                                    
159600     EJECT                                                                
159700 FB-AENDRA-SATSSTRUKTUR SECTION.                                          
159800                                                                          
159900     IF STRUKTURNR-FINNS-KONVERTERAT                                      
160000       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
160100     ELSE                                                                 
160200       MOVE IDARTNR-WS TO W-IDARTNR                                       
160300     END-IF                                                               
160400                                                                          
160500     PERFORM IMS-GET-SATB01                                               
160600                                                                          
160700     IF MID-BEART-IN = ALL '+'                                            
160800       CONTINUE                                                           
160900     ELSE                                                                 
161000       MOVE BEART-SVE-SPAR TO SATB01-STR-BEART-SVE                        
161100     END-IF                                                               
161200                                                                          
161300     IF MID-KDBENHOM-IN = ALL '+'                                         
161400       CONTINUE                                                           
161500     ELSE                                                                 
161600       MOVE MID-KDBENHOM-IN TO SATB01-STR-KDBENHOM                        
161700     END-IF                                                               
161800                                                                          
161900     IF MID-KDPRODSL-IN = ALL '+'                                         
162000       CONTINUE                                                           
162100     ELSE                                                                 
162200       MOVE MID-KDPRODSL-IN TO SATB01-STR-KDPRODSL                        
162300     END-IF                                                               
162400                                                                          
162500     IF MID-IDFKNGRP-IN = ALL '+'                                         
162600       CONTINUE                                                           
162700     ELSE                                                                 
162800       MOVE MID-IDFKNGRP-IN TO SATB01-STR-IDFKNGRP                        
162900     END-IF                                                               
163000                                                                          
163100     IF MID-IDSTRTYP-IN = ALL '+'                                         
163200       CONTINUE                                                           
163300     ELSE                                                                 
163400       MOVE MID-IDSTRTYP-IN TO SATB01-STR-IDSTRTYP                        
163500     END-IF                                                               
163600                                                                          
163700     IF MID-TESTRNOT(1) = ALL '+'                                         
163800       CONTINUE                                                           
163900     ELSE                                                                 
164000       MOVE MID-TESTRNOT(1) TO SATB01-STR-TESTRNOT(1)                     
164100     END-IF                                                               
164200                                                                          
164300     IF MID-TESTRNOT(2) = ALL '+'                                         
164400       CONTINUE                                                           
164500     ELSE                                                                 
164600       MOVE MID-TESTRNOT(2) TO SATB01-STR-TESTRNOT(2)                     
164700     END-IF                                                               
164800                                                                          
164900     PERFORM IMS-REPL-SATB                                                
165000     .                                                                    
165100     EJECT                                                                
165200 FC-NYUPPL-SATSSTRUKTUR SECTION.                                          
165300                                                                          
165400     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
165500                                                                          
165600       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
165700       PERFORM IMS-GET-ARTC01                                             
165800       IF ART-IDLEVNR = '1002 '                                           
165900         MOVE ART-IDLEVNR TO W-IDLEVNR                                    
166000                             SATB01I-STR-IDLEVNR                          
166100       ELSE                                                               
166200         MOVE SPACE TO SATB01I-STR-IDLEVNR                                
166300       END-IF                                                             
166400                                                                          
166500       MOVE ZERO TO  SATB01I-STR-KDBENHOM                                 
166600                     SATB01I-STR-KDPRODSL                                 
166700                     SATB01I-STR-IDFKNGRP                                 
166800       MOVE SPACE TO SATB01I-STR-BEART-SVE                                
166900     ELSE                                                                 
167000       MOVE SPACE           TO SATB01I-STR-IDLEVNR                        
167100       MOVE BEART-SVE-SPAR  TO SATB01I-STR-BEART-SVE                      
167200       MOVE MID-KDPRODSL-IN TO KDPRODSL-WS                                
167300       MOVE KDPRODSL-WS     TO SATB01I-STR-KDPRODSL                       
167400       MOVE MID-IDFKNGRP-IN TO IDFKNGRP-WS                                
167500       MOVE IDFKNGRP-WS     TO SATB01I-STR-IDFKNGRP                       
167600                                                                          
167700       IF MID-KDBENHOM-IN = ALL '+'                                       
167800         MOVE ZERO            TO SATB01I-STR-KDBENHOM                     
167900       ELSE                                                               
168000         MOVE MID-KDBENHOM-IN TO SATB01I-STR-KDBENHOM                     
168100       END-IF                                                             
168200     END-IF                                                               
168300                                                                          
168400     MOVE IDARTNR-NYUPPL-WS TO SATB01I-STR-IDARTNR                        
168500     MOVE SPACE             TO SATB01I-STR-FLEXFORP                       
168600     MOVE 'N'               TO SATB01I-STR-FLFORPQ                        
168700     MOVE MID-IDSTRTYP-IN   TO SATB01I-STR-IDSTRTYP                       
168800     MOVE MSG-SIGNON-USERID TO SATB01I-STR-IDUSER                         
168900     MOVE DAGENS-DATUM      TO SATB01I-STR-TIREGDAT                       
169000     MOVE ZERO              TO SATB01I-STR-TIUPPDAT                       
169100                               SATB01I-STR-TIBORT                         
169200                               SATB01I-STR-KVBYGMIN                       
169300                                                                          
169400     MOVE SPACE             TO SATB01I-STR-IDTSPEC                        
169500                                                                          
169600     IF MID-TESTRNOT(1) = ALL '+'                                         
169700       MOVE SPACE           TO SATB01I-STR-TESTRNOT(1)                    
169800     ELSE                                                                 
169900       MOVE MID-TESTRNOT(1) TO SATB01I-STR-TESTRNOT(1)                    
170000     END-IF                                                               
170100                                                                          
170200     IF MID-TESTRNOT(2) = ALL '+'                                         
170300       MOVE SPACE           TO SATB01I-STR-TESTRNOT(2)                    
170400     ELSE                                                                 
170500       MOVE MID-TESTRNOT(2) TO SATB01I-STR-TESTRNOT(2)                    
170600     END-IF                                                               
170700                                                                          
170800     PERFORM IMS-ISRT-SATB01                                              
170900     MOVE FINNS TO STRUKTURNR-TYP                                         
171000     .                                                                    
171100     EJECT                                                                
171200 FD-KOPIERA-SATSSTRUKTUR SECTION.                                         
171300                                                                          
171400     MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                  
171500     PERFORM IMS-GET-SATB01                                               
171600     IF SEGMENT-FINNS                                                     
171700****   STRUKTUR FINNS REDAN SOM ROT                                       
171800****   KOPIERA BEFINTLIGT STRUKTURHUVUD                                   
171900                                                                          
172000       MOVE IO-AREA                       TO IO-AREA2                     
172100       MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO SATB01I-STR-IDARTNR          
172200       MOVE SPACE                         TO SATB01I-STR-FLEXFORP         
172300       MOVE 'N'                           TO SATB01I-STR-FLFORPQ          
172400       MOVE MSG-SIGNON-USERID             TO SATB01I-STR-IDUSER           
172500       MOVE DAGENS-DATUM                  TO SATB01I-STR-TIREGDAT         
172600       MOVE ZERO                          TO SATB01I-STR-TIUPPDAT         
172700                                             SATB01I-STR-TIBORT           
172800                                             SATB01I-STR-KVBYGMIN         
172900                                                                          
173000       PERFORM IMS-ISRT-SATB01                                            
173100                                                                          
173200*****  LÄS IN ROT PÅ STRUKTUR SOM SKALL KOPIERAS                          
173300       MOVE IDARTNR-WS TO W-IDARTNR                                       
173400       PERFORM IMS-GET-SATB01                                             
173500     ELSE                                                                 
173600       MOVE IDARTNR-WS TO W-IDARTNR                                       
173700       PERFORM IMS-GET-SATB01                                             
173800                                                                          
173900************   KOPIERA STRUKTURHUVUD                                      
174000                                                                          
174100       MOVE IO-AREA                       TO IO-AREA2                     
174200       MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO SATB01I-STR-IDARTNR          
174300       MOVE SPACE                         TO SATB01I-STR-FLEXFORP         
174400       MOVE 'N'                           TO SATB01I-STR-FLFORPQ          
174500       MOVE MSG-SIGNON-USERID             TO SATB01I-STR-IDUSER           
174600       MOVE DAGENS-DATUM                  TO SATB01I-STR-TIREGDAT         
174700       MOVE ZERO                          TO SATB01I-STR-TIUPPDAT         
174800                                             SATB01I-STR-TIBORT           
174900                                             SATB01I-STR-KVBYGMIN         
175000                                                                          
175100       IF STRUKTURNR-NY-FINNS-PAA-ARTREG                                  
175200******   OM DET NYA STRUKTURNUMRET FINNS PÅ ARTREG                        
175300         PERFORM FDA-NOLLA-ARTUPPG                                        
175400         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
175500         PERFORM IMS-GET-ARTC01                                           
175600         IF ART-IDLEVNR = '1002 '                                         
175700           MOVE ART-IDLEVNR TO SATB01I-STR-IDLEVNR                        
175800         ELSE                                                             
175900           MOVE SPACE TO SATB01I-STR-IDLEVNR                              
176000         END-IF                                                           
176100                                                                          
176200       ELSE                                                               
176300         IF STRUKTURNR-FINNS-PAA-ARTREG                                   
176400******   OM DET KOPIERADE STRUKTURNUMRET FINNS PÅ ARTREG                  
176500           MOVE IDARTNR-WS TO W-IDARTNR                                   
176600           PERFORM FDB-HAMTA-ARTUPPG                                      
176700           MOVE SPACE TO SATB01I-STR-IDLEVNR                              
176800                                                                          
176900           IF MID-BEART-IN = ALL '+'                                      
177000             CONTINUE                                                     
177100           ELSE                                                           
177200             MOVE BEART-SVE-SPAR TO SATB01I-STR-BEART-SVE                 
177300           END-IF                                                         
177400                                                                          
177500           IF MID-KDBENHOM-IN = ALL '+'                                   
177600             CONTINUE                                                     
177700           ELSE                                                           
177800             MOVE MID-KDBENHOM-IN TO SATB01I-STR-KDBENHOM                 
177900           END-IF                                                         
178000                                                                          
178100           IF MID-KDPRODSL-IN = ALL '+'                                   
178200             CONTINUE                                                     
178300           ELSE                                                           
178400             MOVE MID-KDPRODSL-IN TO SATB01I-STR-KDPRODSL                 
178500           END-IF                                                         
178600                                                                          
178700           IF MID-IDFKNGRP-IN = ALL '+'                                   
178800             CONTINUE                                                     
178900           ELSE                                                           
179000             MOVE MID-IDFKNGRP-IN TO SATB01I-STR-IDFKNGRP                 
179100           END-IF                                                         
179200                                                                          
179300         ELSE                                                             
179400*          *** BEHÅLL ARTIKELUPPG FRÅN RASA ***                           
179500                                                                          
179600           MOVE SPACE TO SATB01-STR-IDLEVNR                               
179700                                                                          
179800           IF MID-BEART-IN = ALL '+'                                      
179900             CONTINUE                                                     
180000           ELSE                                                           
180100             MOVE BEART-SVE-SPAR TO SATB01I-STR-BEART-SVE                 
180200           END-IF                                                         
180300                                                                          
180400           IF MID-KDBENHOM-IN = ALL '+'                                   
180500             CONTINUE                                                     
180600           ELSE                                                           
180700             MOVE MID-KDBENHOM-IN TO SATB01I-STR-KDBENHOM                 
180800           END-IF                                                         
180900                                                                          
181000           IF MID-KDPRODSL-IN = ALL '+'                                   
181100             CONTINUE                                                     
181200           ELSE                                                           
181300             MOVE MID-KDPRODSL-IN TO SATB01I-STR-KDPRODSL                 
181400           END-IF                                                         
181500                                                                          
181600           IF MID-IDFKNGRP-IN = ALL '+'                                   
181700             CONTINUE                                                     
181800           ELSE                                                           
181900             MOVE MID-IDFKNGRP-IN TO SATB01I-STR-IDFKNGRP                 
182000           END-IF                                                         
182100                                                                          
182200         END-IF                                                           
182300       END-IF                                                             
182400                                                                          
182500       MOVE SPACE TO SATB01I-STR-IDTSPEC                                  
182600                                                                          
182700       IF MID-IDSTRTYP-IN = ALL '+'                                       
182800         CONTINUE                                                         
182900       ELSE                                                               
183000         MOVE MID-IDSTRTYP-IN TO SATB01I-STR-IDSTRTYP                     
183100       END-IF                                                             
183200                                                                          
183300       IF MID-TESTRNOT(1) = ALL '+'                                       
183400         CONTINUE                                                         
183500       ELSE                                                               
183600         MOVE MID-TESTRNOT(1) TO SATB01I-STR-TESTRNOT(1)                  
183700       END-IF                                                             
183800                                                                          
183900       IF MID-TESTRNOT(2) = ALL '+'                                       
184000         MOVE MFS-ROER-EJ-FAELT TO MOD-TESTRNOT(2)                        
184100       ELSE                                                               
184200         MOVE MID-TESTRNOT(2) TO SATB01I-STR-TESTRNOT(2)                  
184300       END-IF                                                             
184400                                                                          
184500       PERFORM IMS-ISRT-SATB01                                            
184600     END-IF                                                               
184700                                                                          
184800                                                                          
184900************ KOPIERA STRUKTURRADER                                        
185000     MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO W-IDARTNR-I                    
185100                                                                          
185200     PERFORM IMS-GET-SATB11                                               
185300     IF SEGMENT-FINNS                                                     
185400       PERFORM UNTIL RADSEGMENT-SLUT                                      
185500                                                                          
185600         MOVE SATB11-RAD-TISTODAT  TO TMP1-YYMMDD                         
185700         MOVE DAGENS-DATUM         TO TMP2-YYMMDD                         
185800         PERFORM WY2000P1                                                 
185900         IF (SATB11-RAD-KDSTRRAD NOT = '0') OR                            
186000             (SATB11-RAD-KDISATS = ('E' OR 'U') ) OR                      
186100              (TMP1-YYMMDD < TMP2-YYMMDD)                                 
186200           CONTINUE                                                       
186300         ELSE                                                             
186400           PERFORM FDC-FLYTTA-RADINFO                                     
186500           PERFORM IMS-ISRT-SATB11                                        
186600                                                                          
186700************   KOPIERA STRUKTURNOTERINGAR                                 
186800                                                                          
186900           PERFORM IMS-GET-SATB22                                         
187000           IF SEGMENT-FINNS                                               
187100             PERFORM UNTIL NOTSEGMENT-SLUT                                
187200               PERFORM FDE-FLYTTA-NOTINFO                                 
187300               PERFORM IMS-ISRT-SATB22                                    
187400               PERFORM IMS-GET-SATB22                                     
187500               MOVE SATB-STATUS-CODE TO STATUS-NOT-WS                     
187600             END-PERFORM                                                  
187700           END-IF                                                         
187800         END-IF                                                           
187900                                                                          
188000         PERFORM IMS-GET-SATB11                                           
188100         MOVE SATB-STATUS-CODE TO STATUS-ART-WS                           
188200       END-PERFORM                                                        
188300                                                                          
188400     END-IF                                                               
188500                                                                          
188600**** SWITCHAR SÄTTS RÄTT FÖR ATT FUNKA VID G-LAES...                      
188700**** DEN NYA STRUKTUREN (KONVERTERADE) SKALL VISAS                        
188800     MOVE IDARTNR-NYUPPL-WS             TO IDARTNR-WS                     
188900     MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO IDARTNR-KONVERTERAT-WS         
189000**** DEN NYA STRUKTUREN FINNS                                             
189100     MOVE FINNS TO STRUKTURNR-TYP                                         
189200     MOVE NEJ   TO FINNS-OKONVERTERAT-SW                                  
189300     MOVE JA    TO FINNS-KONVERTERAT-SW                                   
189400     IF STRUKTURNR-NY-FINNS-PAA-ARTREG                                    
189500       MOVE FINNS TO STRUKTURNR-PAA-ARTREG                                
189600     ELSE                                                                 
189700       MOVE SAKNAS TO STRUKTURNR-PAA-ARTREG                               
189800     END-IF                                                               
189900     .                                                                    
190000     EJECT                                                                
190100 FDA-NOLLA-ARTUPPG SECTION.                                               
190200                                                                          
190300     MOVE SPACE TO SATB01I-STR-BEART-SVE                                  
190400     MOVE ZERO  TO SATB01I-STR-IDFKNGRP                                   
190500                   SATB01I-STR-KDBENHOM                                   
190600                   SATB01I-STR-KDPRODSL                                   
190700     .                                                                    
190800                                                                          
190900 FDB-HAMTA-ARTUPPG SECTION.                                               
191000                                                                          
191100     MOVE IDARTNR-WS TO W-IDARTNR                                         
191200     PERFORM IMS-GET-ARTC01                                               
191300     MOVE ART-IDFKNGRP    TO SATB01I-STR-IDFKNGRP                         
191400     MOVE ART-KDPRODSL    TO SATB01I-STR-KDPRODSL                         
191500     MOVE BEART-SVE-SPAR  TO SATB01I-STR-BEART-SVE                        
191600     MOVE KDBENHOM-SPAR   TO SATB01I-STR-KDBENHOM                         
191700     .                                                                    
191800     EJECT                                                                
191900 FDC-FLYTTA-RADINFO SECTION.                                              
192000                                                                          
192100     MOVE SATB11-RAD-KDSTRRAD  TO SATB11I-RAD-KDSTRRAD                    
192200                                  W-KDSTRRAD                              
192300     MOVE SATB11-RAD-IDRADNR   TO SATB11I-RAD-IDRADNR                     
192400                                  W-IDRADNR                               
192500     MOVE SATB11-RAD-IDLEVNR   TO SATB11I-RAD-IDLEVNR                     
192600     MOVE SATB11-RAD-BELEVART  TO SATB11I-RAD-BELEVART                    
192700     MOVE SATB11-RAD-IDARTNR   TO SATB11I-RAD-IDARTNR                     
192800     MOVE SATB11-RAD-BEART-SVE TO SATB11I-RAD-BEART-SVE                   
192900     MOVE SPACE                TO SATB11I-RAD-IDAO-STA                    
193000                                  SATB11I-RAD-IDAO-STO                    
193100     MOVE SATB11-RAD-IDSTRTYP  TO SATB11I-RAD-IDSTRTYP                    
193200     MOVE SATB11-RAD-KDBENHOM  TO SATB11I-RAD-KDBENHOM                    
193300     MOVE 'N'                  TO SATB11I-RAD-KDISATS                     
193400     MOVE SATB11-RAD-KDSORT    TO SATB11I-RAD-KDSORT                      
193500     MOVE SATB11-RAD-REANTPSA  TO SATB11I-RAD-REANTPSA                    
193600     MOVE DAGENS-DATUM         TO SATB11I-RAD-TIREGDAT                    
193700                                                                          
193800     MOVE SATB11-RAD-TISTADAT   TO TMP1-YYMMDD                            
193900     MOVE DAGENS-DATUM          TO TMP2-YYMMDD                            
194000     PERFORM WY2000P1                                                     
194100     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
194200       MOVE SATB11-RAD-TISTADAT TO SATB11I-RAD-TISTADAT                   
194300     ELSE                                                                 
194400       MOVE DAGENS-DATUM      TO SATB11I-RAD-TISTADAT                     
194500     END-IF                                                               
194600                                                                          
194700     MOVE +999999            TO SATB11I-RAD-TISTODAT                      
194800                                                                          
194900     .                                                                    
195000                                                                          
195100 FDE-FLYTTA-NOTINFO SECTION.                                              
195200                                                                          
195300     MOVE SATB22-NOT-IDSTRNOT    TO SATB22I-NOT-IDSTRNOT                  
195400     MOVE SATB22-NOT-TESTRNOT(1) TO SATB22I-NOT-TESTRNOT(1)               
195500     MOVE SATB22-NOT-TESTRNOT(2) TO SATB22I-NOT-TESTRNOT(2)               
195600                                                                          
195700     .                                                                    
195800     EJECT                                                                
195900 G-LAES-VISA-STRUKTUR SECTION.                                            
196000                                                                          
196100     MOVE IDARTNR-WS TO W-IDARTNR                                         
196200     IF STRUKTURNR-SAKNAS                                                 
196300       IF STRUKTURNR-FINNS-PAA-ARTREG                                     
196400         PERFORM GA-VIS-INF-FRAN-ARTC-BENA-WDF5                           
196500       ELSE                                                               
196600         PERFORM MFS-RENSA-FAELT-IN                                       
196700         PERFORM MFS-RENSA-FAELT-UT                                       
196800         PERFORM MFS-RENSA-FAELT-BADE-IN-O-UT                             
196900       END-IF                                                             
197000     ELSE                                                                 
197100       IF STRUKTURNR-FINNS-PAA-ARTREG                                     
197200         PERFORM GB-INF-FRA-ARTC-BENA-WDF5-SATB                           
197300       ELSE                                                               
197400         PERFORM GC-VISA-ALL-INF-FRAN-SATB                                
197500       END-IF                                                             
197600     END-IF                                                               
197700                                                                          
197800     PERFORM MFS-RENSA-FAELT-IN                                           
197900                                                                          
198000     IF STRUKTURNR-FINNS                                                  
198100       PERFORM MFS-FORM-ATTR                                              
198200     ELSE                                                                 
198300       PERFORM MFS-FORM-ATTR                                              
198400       PERFORM S06-STAENG-EVENTUELLA-FAELT                                
198500     END-IF                                                               
198600                                                                          
198700     .                                                                    
198800     SKIP2                                                                
198900 GA-VIS-INF-FRAN-ARTC-BENA-WDF5 SECTION.                                  
199000                                                                          
199100     PERFORM MFS-RENSA-FAELT-UT                                           
199200     PERFORM S02-INF-FRA-ARTC-BEN-WDF5                                    
199300     .                                                                    
199400     SKIP2                                                                
199500 GB-INF-FRA-ARTC-BENA-WDF5-SATB SECTION.                                  
199600                                                                          
199700     PERFORM S02-INF-FRA-ARTC-BEN-WDF5                                    
199800                                                                          
199900     IF (STRUKTURNR-FINNS-KONVERTERAT) AND                                
200000         (NOT STRUKTURNR-SPAERRAT)                                        
200100****** OM STRUKTUNUMRET FINNS I KONVERTERAD FORM OCH ÄR                   
200200****** 'TILLÅTEN', VISAS INFO FRÅN DEN KOVERTERADE FORMEN                 
200300       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
200400     ELSE                                                                 
200500****** VISAS INFO FRÅN DEN 'RIKTIGA' FORMEN                               
200600       MOVE IDARTNR-WS TO W-IDARTNR                                       
200700     END-IF                                                               
200800                                                                          
200900     PERFORM IMS-GET-SATB01                                               
201000     MOVE SATB01-STR-IDSTRTYP    TO MOD-IDSTRTYP-UT                       
201100     MOVE SATB01-STR-TIREGDAT    TO MOD-TIREGDAT                          
201200                                                                          
201300     IF SATB01-STR-TESTRNOT(1) = SPACE                                    
201400       MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT(1)                            
201500     ELSE                                                                 
201600       MOVE SATB01-STR-TESTRNOT(1) TO MOD-TESTRNOT(1)                     
201700     END-IF                                                               
201800                                                                          
201900     IF SATB01-STR-TESTRNOT(2) = SPACE                                    
202000       MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT(2)                            
202100     ELSE                                                                 
202200       MOVE SATB01-STR-TESTRNOT(2) TO MOD-TESTRNOT(2)                     
202300     END-IF                                                               
202400     .                                                                    
202500     SKIP2                                                                
202600 GC-VISA-ALL-INF-FRAN-SATB SECTION.                                       
202700                                                                          
202800     IF (STRUKTURNR-FINNS-KONVERTERAT) AND                                
202900         (NOT STRUKTURNR-SPAERRAT)                                        
203000****** OM STRUKTUNUMRET FINNS I KONVERTERAD FORM OCH ÄR                   
203100****** 'TILLÅTEN', VISAS INFO FRÅN DEN KOVERTERADE FORMEN                 
203200       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
203300     ELSE                                                                 
203400****** ANNARS VISAS INFO FRÅN DEN 'RIKTIGA' FORMEN                        
203500       MOVE IDARTNR-WS TO W-IDARTNR                                       
203600     END-IF                                                               
203700                                                                          
203800     PERFORM IMS-GET-SATB01                                               
203900     MOVE SATB01-STR-IDFKNGRP    TO MOD-IDFKNGRP-UT                       
204000     MOVE SATB01-STR-IDLEVNR     TO MOD-IDLEVNR                           
204100     MOVE SATB01-STR-IDSTRTYP    TO MOD-IDSTRTYP-UT                       
204200     MOVE SATB01-STR-KDBENHOM    TO MOD-KDBENHOM-UT                       
204300                                    KDBENHOM-RASA-SPAR                    
204400     MOVE SATB01-STR-KDPRODSL    TO MOD-KDPRODSL-UT                       
204500     MOVE SATB01-STR-TIREGDAT    TO MOD-TIREGDAT                          
204600                                                                          
204700     IF SATB01-STR-TESTRNOT(1) = SPACE                                    
204800       MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT(1)                            
204900     ELSE                                                                 
205000       MOVE SATB01-STR-TESTRNOT(1) TO MOD-TESTRNOT(1)                     
205100     END-IF                                                               
205200     IF SATB01-STR-TESTRNOT(2) = SPACE                                    
205300       MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT(2)                            
205400     ELSE                                                                 
205500       MOVE SATB01-STR-TESTRNOT(2) TO MOD-TESTRNOT(2)                     
205600     END-IF                                                               
205700                                                                          
205800     IF IDSKYLT-WS = 'S  '                                                
205900       MOVE SATB01-STR-BEART-SVE TO MOD-BEART-UT                          
206000     ELSE                                                                 
206100****** HÄMTA UTLÄNDSK BENÄMNING                                           
206200       MOVE 'S  ' TO W-IDSKYLT                                            
206300       MOVE SATB01-STR-BEART-SVE TO W-BEART                               
206400       PERFORM IMS-GET-BENA01-ASEQ                                        
206500       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
206600                     BENA01-BEN-KDHOMONYM = KDBENHOM-RASA-SPAR            
206700         IF SEGMENT-FINNS                                                 
206800           IF BENA01-BEN-KDHOMONYM = KDBENHOM-RASA-SPAR                   
206900             CONTINUE                                                     
207000           ELSE                                                           
207100             PERFORM IMS-GET-BENA01-ASEQ-NEXT                             
207200           END-IF                                                         
207300         ELSE                                                             
207400           MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                           
207500         END-IF                                                           
207600       END-PERFORM                                                        
207700                                                                          
207800       IF SEGMENT-FINNS                                                   
207900         MOVE IDSKYLT-WS TO W-IDSKYLT                                     
208000         PERFORM IMS-GET-BENA11-ASEQ                                      
208100         IF SEGMENT-FINNS                                                 
208200           MOVE BENA11-TEXT-BEART TO MOD-BEART-UT                         
208300         ELSE                                                             
208400           MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                           
208500         END-IF                                                           
208600       END-IF                                                             
208700     END-IF                                                               
208800     .                                                                    
208900     EJECT                                                                
209000 H-SAMMA-SIDA SECTION.                                                    
209100                                                                          
209200     PERFORM MFS-ROER-EJ-FAELT-IN                                         
209300     PERFORM MFS-ROER-EJ-FAELT-UT                                         
209400     PERFORM MFS-ROER-EJ-FAELT-BADE-IN-O-UT                               
209500                                                                          
209600     IF MID-BORT = SPACE                                                  
209700****** BEHANDLAS SOM EJ IFYLLD                                            
209800       MOVE '+' TO MID-BORT                                               
209900     END-IF                                                               
210000                                                                          
210100     IF MID-BEART-IN = SPACE                                              
210200****** BEHANDLAS SOM EJ IFYLLD                                            
210300       MOVE '+++++++++++++++++++++++++' TO MID-BEART-IN                   
210400     END-IF                                                               
210500                                                                          
210600     IF MID-IDSTRTYP-IN = SPACE                                           
210700****** BEHANDLAS SOM EJ IFYLLD                                            
210800       MOVE '+' TO MID-IDSTRTYP-IN                                        
210900     END-IF                                                               
211000                                                                          
211100     IF MID-INPUT NOT = ALL '+'                                           
211200       MOVE MED6(SPRAK-IX) TO MOD-TEMFSFEL                                
211300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
211400       PERFORM MFS-LAES-IN-IGEN                                           
211500     END-IF                                                               
211600                                                                          
211700     PERFORM S06-STAENG-EVENTUELLA-FAELT                                  
211800     .                                                                    
211900     EJECT                                                                
212000 S01-KOLLA-FINNS-PA-ARTREG-INPU SECTION.                                  
212100                                                                          
212200     IF (MID-BEART-IN = ALL '+')                                          
212300       CONTINUE                                                           
212400     ELSE                                                                 
212500       MOVE NEJ                TO INDATA-SW                               
212600       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                       
212700     END-IF                                                               
212800                                                                          
212900     IF (MID-KDBENHOM-IN = ALL '+')                                       
213000       CONTINUE                                                           
213100     ELSE                                                                 
213200       MOVE NEJ               TO INDATA-SW                                
213300       MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                     
213400     END-IF                                                               
213500                                                                          
213600     IF (MID-KDPRODSL-IN = ALL '+')                                       
213700       CONTINUE                                                           
213800     ELSE                                                                 
213900       MOVE NEJ               TO INDATA-SW                                
214000       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                     
214100     END-IF                                                               
214200                                                                          
214300     IF (MID-IDFKNGRP-IN = ALL '+')                                       
214400       CONTINUE                                                           
214500     ELSE                                                                 
214600       MOVE NEJ               TO INDATA-SW                                
214700       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                     
214800     END-IF                                                               
214900                                                                          
215000     IF UPDATE-AENDRA                                                     
215100       PERFORM S011-KOLLA-IDSTRTYP-AENDRA                                 
215200     ELSE                                                                 
215300       IF UPDATE-NYUPPL                                                   
215400         PERFORM S012-KOLLA-IDSTRTYP-NYUPPL                               
215500       ELSE                                                               
215600********* KOPIERING                                                       
215700         PERFORM S013-KOLLA-IDSTRTYP-NYUPPL-KOP                           
215800       END-IF                                                             
215900     END-IF                                                               
216000     .                                                                    
216100     EJECT                                                                
216200 S011-KOLLA-IDSTRTYP-AENDRA SECTION.                                      
216300***********************************************                           
216400* EVENTUELL INMATAD STRUKTURTYP MÅSTE 'STÄMMA'*                           
216500* MED SORT PÅ ARTREG.                         *                           
216600***********************************************                           
216700                                                                          
216800     IF (MID-IDSTRTYP-IN = ALL '+')                                       
216900       CONTINUE                                                           
217000     ELSE                                                                 
217100       IF (MID-IDSTRTYP-IN = 'S')                                         
217200         MOVE IDARTNR-WS TO W-IDARTNR                                     
217300         PERFORM IMS-GET-ARTC01                                           
217400         IF ART-KDSORT = 'SA' OR 'TM'                                     
217500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
217600         ELSE                                                             
217700           MOVE NEJ                TO INDATA-SW                           
217800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
217900         END-IF                                                           
218000       ELSE                                                               
218100         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
218200           MOVE IDARTNR-WS TO W-IDARTNR                                   
218300           PERFORM IMS-GET-ARTC01                                         
218400           IF ART-KDSORT = 'ST'                                           
218500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
218600           ELSE                                                           
218700             MOVE NEJ                TO INDATA-SW                         
218800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
218900           END-IF                                                         
219000         ELSE                                                             
219100           MOVE NEJ                TO INDATA-SW                           
219200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
219300         END-IF                                                           
219400       END-IF                                                             
219500     END-IF                                                               
219600     .                                                                    
219700     EJECT                                                                
219800 S012-KOLLA-IDSTRTYP-NYUPPL SECTION.                                      
219900***********************************************                           
220000* IMATAD STRUKTURTYP MÅSTE 'STÄMMA' MED SORT  *                           
220100* PÅ ARTREG.                                  *                           
220200***********************************************                           
220300                                                                          
220400     IF (MID-IDSTRTYP-IN = ALL '+')                                       
220500       MOVE NEJ                TO INDATA-SW                               
220600       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                    
220700     ELSE                                                                 
220800       IF (MID-IDSTRTYP-IN = 'S')                                         
220900         MOVE IDARTNR-WS TO W-IDARTNR                                     
221000         PERFORM IMS-GET-ARTC01                                           
221100         IF ART-KDSORT = 'SA' OR 'TM'                                     
221200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
221300         ELSE                                                             
221400           MOVE NEJ                TO INDATA-SW                           
221500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
221600         END-IF                                                           
221700       ELSE                                                               
221800         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
221900           MOVE IDARTNR-WS TO W-IDARTNR                                   
222000           PERFORM IMS-GET-ARTC01                                         
222100           IF ART-KDSORT = 'ST'                                           
222200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
222300           ELSE                                                           
222400             MOVE NEJ                TO INDATA-SW                         
222500             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
222600           END-IF                                                         
222700         ELSE                                                             
222800           MOVE NEJ                TO INDATA-SW                           
222900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
223000         END-IF                                                           
223100       END-IF                                                             
223200     END-IF                                                               
223300     .                                                                    
223400     EJECT                                                                
223500 S013-KOLLA-IDSTRTYP-NYUPPL-KOP SECTION.                                  
223600***********************************************                           
223700* IMATAD IDSTRTYP MÅSTE 'STÄMMA' MED SORT     *                           
223800* PÅ ARTREG. OM EJ INMATAD STRUKTURTYP MÅSTE  *                           
223900* DEN KOPIERADE STRUKTURENS IDSTRTYP 'STÄMMA' *                           
224000* MED SORT PÅ ARTREG.                         *                           
224100***********************************************                           
224200                                                                          
224300     IF (MID-IDSTRTYP-IN = ALL '+')                                       
224400*****  KOLLA KOPIERAD STRUKTURS IDSTRTYP                                  
224500       MOVE IDARTNR-WS TO W-IDARTNR                                       
224600       PERFORM IMS-GET-SATB01                                             
224700       IF SATB01-STR-IDSTRTYP = 'S'                                       
224800         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
224900         PERFORM IMS-GET-ARTC01                                           
225000         IF ART-KDSORT = 'SA' OR 'TM'                                     
225100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
225200         ELSE                                                             
225300           MOVE NEJ                TO INDATA-SW                           
225400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
225500         END-IF                                                           
225600       ELSE                                                               
225700******* IDSTRTYP = R ELLER K                                              
225800         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
225900         PERFORM IMS-GET-ARTC01                                           
226000         IF ART-KDSORT = 'ST'                                             
226100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
226200         ELSE                                                             
226300           MOVE NEJ                TO INDATA-SW                           
226400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
226500         END-IF                                                           
226600       END-IF                                                             
226700     ELSE                                                                 
226800*****  KOLLA INMATAD IDSTRTYP                                             
226900       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
227000       IF (MID-IDSTRTYP-IN = 'S')                                         
227100         PERFORM IMS-GET-ARTC01                                           
227200         IF ART-KDSORT = 'SA' OR 'TM'                                     
227300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
227400         ELSE                                                             
227500           MOVE NEJ                TO INDATA-SW                           
227600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
227700         END-IF                                                           
227800       ELSE                                                               
227900         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
228000           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
228100           PERFORM IMS-GET-ARTC01                                         
228200           IF ART-KDSORT = 'ST'                                           
228300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
228400           ELSE                                                           
228500             MOVE NEJ                TO INDATA-SW                         
228600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
228700           END-IF                                                         
228800         ELSE                                                             
228900           MOVE NEJ                TO INDATA-SW                           
229000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
229100         END-IF                                                           
229200       END-IF                                                             
229300     END-IF                                                               
229400     .                                                                    
229500     EJECT                                                                
229600 S02-INF-FRA-ARTC-BEN-WDF5 SECTION.                                       
229700******************************************                                
229800*     HÄMTA UPPGIFTER FRÅN ARTC          *                                
229900******************************************                                
230000                                                                          
230100     PERFORM IMS-GET-ARTC01                                               
230200     MOVE ART-IDLEVNR  TO IDLEVNR-ART-WS                                  
230300     MOVE ART-IDFKNGRP TO MOD-IDFKNGRP-UT                                 
230400     MOVE ART-KDPRODSL TO MOD-KDPRODSL-UT                                 
230500                                                                          
230600     MOVE +1 TO INDX                                                      
230700     PERFORM UNTIL INDX > MAX-IDAO                                        
230800       MOVE ART-IDAO(INDX) TO MOD-IDAO(INDX)                              
230900       ADD +1 TO INDX                                                     
231000     END-PERFORM                                                          
231100                                                                          
231200******************************************                                
231300                                                                          
231400     PERFORM IMS-GET-ARTC11                                               
231500     IF SEGMENT-FINNS                                                     
231600       MOVE CLAG-KDERS TO MOD-KDERS                                       
231700     ELSE                                                                 
231800       MOVE ZERO TO MOD-KDERS                                             
231900     END-IF                                                               
232000                                                                          
232100     MOVE +3 TO W-KDNOTTYP                                                
232200     PERFORM IMS-GET-ARTC25                                               
232300     IF SEGMENT-FINNS                                                     
232400       MOVE NOT-TEARTNOT TO MOD-TEARTNOT                                  
232500     ELSE                                                                 
232600       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                               
232700     END-IF                                                               
232800                                                                          
232900     MOVE +7 TO W-KDNOTTYP                                                
233000     PERFORM IMS-GET-ARTC25                                               
233100     IF SEGMENT-FINNS                                                     
233200       MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                                
233300     ELSE                                                                 
233400       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                             
233500     END-IF                                                               
233600                                                                          
233700*     HÄMTA UPPGIFTER FRÅN BENA          *                                
233800******************************************                                
233900                                                                          
234000     PERFORM IMS-GET-BENA01-BSEQ                                          
234100     IF SEGMENT-FINNS                                                     
234200       MOVE BENA01-BEN-KDHOMONYM TO MOD-KDBENHOM-UT                       
234300       MOVE IDSKYLT-WS           TO W-IDSKYLT                             
234400       PERFORM IMS-GET-BENA11-BSEQ                                        
234500       IF SEGMENT-FINNS                                                   
234600         MOVE BENA11-TEXT-BEART TO MOD-BEART-UT                           
234700       ELSE                                                               
234800         MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                             
234900       END-IF                                                             
235000     ELSE                                                                 
235100       MOVE MFS-RENSA-FAELT TO MOD-KDBENHOM-UT                            
235200                               MOD-BEART-UT                               
235300     END-IF                                                               
235400******************************************                                
235500*     HÄMTA UPPGIFTER FRÅN WDF5          *                                
235600******************************************                                
235700                                                                          
235800     PERFORM IMS-GET-WDF501                                               
235900     IF SEGMENT-FINNS                                                     
236000       IF IDLEVNR-ART-WS NOT = SPACE                                      
236100         MOVE IDLEVNR-ART-WS TO W-IDLEVNR                                 
236200                                MOD-IDLEVNR                               
236300*****    HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR                           
236400         PERFORM IMS-GET-WDF502-LAST                                      
236500         IF SEGMENT-FINNS                                                 
236600           MOVE XLEV-BELEVART TO MOD-BELEVART                             
236700         ELSE                                                             
236800           MOVE MFS-RENSA-FAELT TO MOD-BELEVART                           
236900         END-IF                                                           
237000       ELSE                                                               
237100*****    HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR + LEVNR                   
237200*****    FRÅN FÖRSTA PÅTRÄFFADE LEVERANTÖR                                
237300         PERFORM IMS-GET-WDF502-OKVAL                                     
237400         IF SEGMENT-FINNS                                                 
237500           MOVE XLEV-IDLEVNR   TO W-IDLEVNR                               
237600                                  MOD-IDLEVNR                             
237700           MOVE XLEV-BELEVART  TO MOD-BELEVART                            
237800           PERFORM IMS-GET-WDF502-LAST                                    
237900           IF SEGMENT-FINNS                                               
238000***********  OM DET FINNS HÖGRE BENÄMNINGSNUMMER                          
238100             MOVE XLEV-BELEVART TO MOD-BELEVART                           
238200           END-IF                                                         
238300         END-IF                                                           
238400       END-IF                                                             
238500     ELSE                                                                 
238600       MOVE MFS-RENSA-FAELT TO MOD-BELEVART                               
238700     END-IF                                                               
238800     .                                                                    
238900     EJECT                                                                
239000 S03-KOLLA-BEART-KDHOM-AENDRING SECTION.                                  
239100                                                                          
239200     IF (MID-KDBENHOM-IN = ALL '+') AND                                   
239300          (MID-BEART-IN = ALL '+')                                        
239400       CONTINUE                                                           
239500     ELSE                                                                 
239600       IF (MID-KDBENHOM-IN NOT = ALL '+') AND                             
239700            (MID-BEART-IN NOT = ALL '+')                                  
239800*******  BEART + KDBENHOM IFYLLDA                                         
239900         IF MID-KDBENHOM-IN NUMERIC                                       
240000           MOVE IDSKYLT-WS    TO W-IDSKYLT                                
240100           MOVE MID-BEART-IN  TO W-BEART                                  
240200           MOVE MID-KDBENHOM-IN TO KDBENHOM-WS                            
240300         ELSE                                                             
240400           MOVE NEJ               TO INDATA-SW                            
240500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                 
240600         END-IF                                                           
240700       ELSE                                                               
240800                                                                          
240900         IF (MID-KDBENHOM-IN NOT = ALL '+')                               
241000*******    BARA KDBENHOM IFYLLD                                           
241100           IF MID-KDBENHOM-IN NUMERIC                                     
241200             MOVE 'S   '          TO W-IDSKYLT                            
241300             MOVE BEART-RASA-SPAR TO W-BEART                              
241400             MOVE MID-KDBENHOM-IN TO KDBENHOM-WS                          
241500           ELSE                                                           
241600             MOVE NEJ               TO INDATA-SW                          
241700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR               
241800           END-IF                                                         
241900         ELSE                                                             
242000*******    BARA BEART IFYLLD                                              
242100           MOVE IDSKYLT-WS         TO W-IDSKYLT                           
242200           MOVE MID-BEART-IN       TO W-BEART                             
242300           MOVE KDBENHOM-RASA-SPAR TO KDBENHOM-WS                         
242400           END-IF                                                         
242500       END-IF                                                             
242600                                                                          
242700       IF INDATA-OK                                                       
242800         PERFORM IMS-GET-BENA01-ASEQ                                      
242900         IF SEGMENT-FINNS                                                 
243000           IF BENA01-BEN-KDBENSTAT < 2                                    
243100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-IN-ATTR               
243200           ELSE                                                           
243300             MOVE NEJ                TO INDATA-SW                         
243400             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                 
243500           END-IF                                                         
243600         ELSE                                                             
243700           MOVE NEJ                TO INDATA-SW                           
243800           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                   
243900         END-IF                                                           
244000                                                                          
244100         IF INDATA-OK                                                     
244200           IF IDSKYLT-WS = 'S  '                                          
244300             IF (MID-BEART-IN NOT = ALL '+') AND                          
244400                 (MID-KDBENHOM-IN = ALL '+')                              
244500               PERFORM IMS-GET-BENA01-ASEQ-NEXT                           
244600               IF SEGMENT-FINNS                                           
244700*************    BENÄMNING FINNS MED FLERA HOMONYMKODER                   
244800*************    HOMONYMKOD MÅSTE ANGES                                   
244900                 MOVE NEJ               TO INDATA-SW                      
245000                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR           
245100               ELSE                                                       
245200                 MOVE MFS-NUM-FAELT-RAETT TO MOD-BEART-IN-ATTR            
245300               END-IF                                                     
245400             END-IF                                                       
245500           END-IF                                                         
245600         END-IF                                                           
245700                                                                          
245800         IF INDATA-OK                                                     
245900           PERFORM IMS-GET-BENA01-ASEQ                                    
246000           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
246100                           (BENA01-BEN-KDHOMONYM = KDBENHOM-WS)           
246200             IF SEGMENT-FINNS                                             
246300               IF BENA01-BEN-KDHOMONYM = KDBENHOM-WS                      
246400                 CONTINUE                                                 
246500               ELSE                                                       
246600                 PERFORM IMS-GET-BENA01-ASEQ-NEXT                         
246700               END-IF                                                     
246800             END-IF                                                       
246900           END-PERFORM                                                    
247000                                                                          
247100           IF SEGMENT-FINNS                                               
247200             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR             
247300           ELSE                                                           
247400             MOVE NEJ               TO INDATA-SW                          
247500             MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR               
247600           END-IF                                                         
247700         END-IF                                                           
247800                                                                          
247900         IF INDATA-OK                                                     
248000*******************************************                               
248100*   SPARA UNDAN SVENSK ARTIKELBENÄMNING   *                               
248200*******************************************                               
248300           IF IDSKYLT-WS = 'S  '                                          
248400             MOVE MID-BEART-IN TO BEART-SVE-SPAR                          
248500           ELSE                                                           
248600             MOVE 'S  ' TO W-IDSKYLT                                      
248700             PERFORM IMS-GET-BENA11-ASEQ                                  
248800             MOVE BENA11-TEXT-BEART TO BEART-SVE-SPAR                     
248900           END-IF                                                         
249000         END-IF                                                           
249100       END-IF                                                             
249200                                                                          
249300     END-IF                                                               
249400     .                                                                    
249500     EJECT                                                                
249600 S04-KOLLA-BEART-KDHOM-NYUPPL SECTION.                                    
249700                                                                          
249800     IF (MID-BEART-IN = ALL '+')                                          
249900       MOVE NEJ                TO INDATA-SW                               
250000       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                       
250100                                                                          
250200       IF (MID-KDBENHOM-IN = ALL '+')                                     
250300         CONTINUE                                                         
250400       ELSE                                                               
250500         IF MID-KDBENHOM-IN NUMERIC                                       
250600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR               
250700         ELSE                                                             
250800           MOVE NEJ                 TO INDATA-SW                          
250900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR               
251000         END-IF                                                           
251100       END-IF                                                             
251200     ELSE                                                                 
251300       MOVE IDSKYLT-WS   TO W-IDSKYLT                                     
251400       MOVE MID-BEART-IN TO W-BEART                                       
251500       PERFORM IMS-GET-BENA01-ASEQ                                        
251600       IF SEGMENT-FINNS                                                   
251700         IF BENA01-BEN-KDBENSTAT < 2                                      
251800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-IN-ATTR                 
251900                                                                          
252000           IF MID-KDBENHOM-IN = ALL '+'                                   
252100             PERFORM IMS-GET-BENA01-ASEQ-NEXT                             
252200             IF SEGMENT-FINNS                                             
252300************** BENÄMNING FINNS MED FLERA HOMONYMKODER                     
252400************** HOMONYMKOD MÅSTE ANGES                                     
252500               MOVE NEJ                 TO INDATA-SW                      
252600               MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR           
252700             ELSE                                                         
252800               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR           
252900               PERFORM IMS-GET-BENA01-ASEQ                                
253000             END-IF                                                       
253100           ELSE                                                           
253200             IF MID-KDBENHOM-IN NUMERIC                                   
253300               MOVE MID-KDBENHOM-IN TO KDBENHOM-WS                        
253400               PERFORM UNTIL (SEGMENT-SAKNAS) OR                          
253500                     (BENA01-BEN-KDHOMONYM = KDBENHOM-WS)                 
253600                                                                          
253700                 IF SEGMENT-FINNS                                         
253800                   IF BENA01-BEN-KDHOMONYM = KDBENHOM-WS                  
253900                     CONTINUE                                             
254000                   ELSE                                                   
254100                     PERFORM IMS-GET-BENA01-ASEQ-NEXT                     
254200                   END-IF                                                 
254300                 END-IF                                                   
254400               END-PERFORM                                                
254500                                                                          
254600               IF SEGMENT-FINNS                                           
254700                 MOVE MFS-NUM-FAELT-RAETT TO                              
254800                                           MOD-KDBENHOM-IN-ATTR           
254900               ELSE                                                       
255000                 MOVE NEJ               TO INDATA-SW                      
255100                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR           
255200               END-IF                                                     
255300             ELSE                                                         
255400               MOVE NEJ               TO INDATA-SW                        
255500               MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR             
255600             END-IF                                                       
255700           END-IF                                                         
255800                                                                          
255900           IF INDATA-OK                                                   
256000*******************************************                               
256100* SPARA UNDAN SVENSK ARTIKELBENÄMNING     *                               
256200*******************************************                               
256300             IF IDSKYLT-WS = 'S  '                                        
256400               MOVE MID-BEART-IN TO BEART-SVE-SPAR                        
256500             ELSE                                                         
256600               MOVE 'S  ' TO W-IDSKYLT                                    
256700               PERFORM IMS-GET-BENA11-ASEQ                                
256800               MOVE BENA11-TEXT-BEART TO BEART-SVE-SPAR                   
256900             END-IF                                                       
257000           END-IF                                                         
257100         ELSE                                                             
257200           MOVE NEJ                   TO INDATA-SW                        
257300           MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-IN-ATTR                
257400           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-IN-ATTR             
257500         END-IF                                                           
257600       ELSE                                                               
257700         MOVE NEJ                   TO INDATA-SW                          
257800         MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-IN-ATTR                  
257900         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-IN-ATTR               
258000       END-IF                                                             
258100     END-IF                                                               
258200     .                                                                    
258300     EJECT                                                                
258400 S05-KOLL-BEART-KDHOM-KOPIERING SECTION.                                  
258500                                                                          
258600     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
258700****** OM DET KOPIERADE STRUKTURNUMRET FINNS PÅ ARTREG                    
258800                                                                          
258900       MOVE IDSKYLT-WS TO W-IDSKYLT                                       
259000       MOVE IDARTNR-WS TO W-IDARTNR                                       
259100       PERFORM IMS-GET-BENA01-BSEQ                                        
259200       MOVE BENA01-BEN-KDHOMONYM TO KDBENHOM-WS                           
259300                                    KDBENHOM-SPAR                         
259400       MOVE 'S  ' TO W-IDSKYLT                                            
259500       PERFORM IMS-GET-BENA11-BSEQ                                        
259600       MOVE BENA11-TEXT-BEART TO W-BEART                                  
259700                                 BEART-SVE-SPAR                           
259800     ELSE                                                                 
259900****** OM DET KOPIERADE STRUKTURNUMRET SAKNAS PÅ ARTREG                   
260000                                                                          
260100       MOVE BEART-RASA-SPAR    TO W-BEART                                 
260200                                  BEART-SVE-SPAR                          
260300       MOVE KDBENHOM-RASA-SPAR TO KDBENHOM-WS                             
260400                                  KDBENHOM-SPAR                           
260500     END-IF                                                               
260600                                                                          
260700**** KONTROLL AV BEART + KDHOM VID ÄNDRING INNAN KOPIERING                
260800     PERFORM S03-KOLLA-BEART-KDHOM-AENDRING                               
260900     .                                                                    
261000     EJECT                                                                
261100 S06-STAENG-EVENTUELLA-FAELT SECTION.                                     
261200                                                                          
261300     IF STRUKTURNR-SAKNAS                                                 
261400       MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-NY                      
261500                                      MOD-BORT                            
261600       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDARTNR-NY-ATTR                 
261700                                      MOD-BORT-ATTR                       
261800     ELSE                                                                 
261900       IF (STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET)                   
262000         PERFORM MFS-STAENG-FAELT-IN                                      
262100       END-IF                                                             
262200     END-IF                                                               
262300     .                                                                    
262400     EJECT                                                                
262500 S07-KOLLA-INGAAR-I-ANNAN-SATS SECTION.                                   
262600*********************************************************                 
262700* KOLL ATT STRUKTURNR EJ INGÅR SOM RAD I ANNAN STRUKTUR *                 
262800*********************************************************                 
262900                                                                          
263000     MOVE SPACE TO W-IDLEVNR                                              
263100                   W-BELEVART                                             
263200                                                                          
263300     PERFORM IMS-GET-SATB-CSEQ-OKONV-UNIK                                 
263400     IF SEGMENT-FINNS                                                     
263500       PERFORM UNTIL SEGMENT-SAKNAS                                       
263600         IF SATB01C-STR-TIBORT > 0                                        
263700**********   STRUKTUR BORTTAGSMÄRKT                                       
263800           PERFORM IMS-GET-SATB-CSEQ-OKONV-NEXT                           
263900         ELSE                                                             
264000           MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                     
264100           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
264200           PERFORM WY2000P1                                               
264300           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
264400**********   STRUKTURNR FINNS SOM GÄLLANDE RAD I ANNAN STRUKTUR           
264500             MOVE 'GE' TO STATUS-WS                                       
264600             MOVE NEJ TO INDATA-SW                                        
264700           ELSE                                                           
264800             PERFORM IMS-GET-SATB-CSEQ-OKONV-NEXT                         
264900           END-IF                                                         
265000         END-IF                                                           
265100       END-PERFORM                                                        
265200     END-IF                                                               
265300                                                                          
265400     IF INDATA-OK                                                         
265500       PERFORM IMS-GET-SATB-CSEQ-KONV-UNIK                                
265600       PERFORM UNTIL SEGMENT-SAKNAS                                       
265700         IF SEGMENT-FINNS                                                 
265800           MOVE 001                  TO WORK-KDCALL                       
265900           MOVE WC-CDC-SE            TO WORK-IDDC                         
266000           MOVE SATB01C-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                 
266100           MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                 
266200           CALL WORKDAY USING WORK-KDCALL                                 
266300                              WORK-DATE-AREA                              
266400                              WORK-KDSVAR                                 
266500           IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                     
266600***********  OM MAN TRÄFFAR PÅ EN KOVERTERAD STRUKTUR                     
266700***********  SOM ÄR ÄLDRE ÄN 2 IGNORERAS DEN.                             
266800***********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.              
266900             PERFORM IMS-GET-SATB-CSEQ-KONV-NEXT                          
267000           ELSE                                                           
267100***********  STRUKTUR FINNS SOM RAD I GÄLLANDE KONV. STRUKTUR             
267200             MOVE 'GE' TO STATUS-WS                                       
267300             MOVE NEJ TO INDATA-SW                                        
267400           END-IF                                                         
267500         END-IF                                                           
267600       END-PERFORM                                                        
267700     END-IF                                                               
267800     .                                                                    
267900     EJECT                                                                
268000 S08-KOLLA-OM-USER-HAR-LAASNING SECTION.                                  
268100**************************************                                    
268200* KOLL OM IDUSER HAR LÅSNING PÅ WDR5 *                                    
268300**************************************                                    
268400                                                                          
268500     MOVE '1151'    TO W-IDHTYP                                           
268600     MOVE LOW-VALUE TO W-LOW-VALUE                                        
268700     PERFORM IMS-GET-XXAZ01                                               
268800                                                                          
268900     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
269000     PERFORM IMS-GET-XXAZ11-IDUSER                                        
269100     PERFORM UNTIL SEGMENT-SAKNAS                                         
269200       IF SEGMENT-FINNS                                                   
269300                                                                          
269400         MOVE 001                  TO WORK-KDCALL                         
269500         MOVE WC-CDC-SE            TO WORK-IDDC                           
269600         MOVE XXAZ11-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                   
269700         MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                   
269800         CALL WORKDAY USING WORK-KDCALL                                   
269900                            WORK-DATE-AREA                                
270000                            WORK-KDSVAR                                   
270100         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
270200********** OM MAN TRÄFFAR PÅ EN 'LÅSNING'                                 
270300********** SOM ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                          
270400********** ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
270500           PERFORM IMS-DLET-XXAZ11                                        
270600           PERFORM IMS-GET-XXAZ11-IDUSER                                  
270700         ELSE                                                             
270800           MOVE 'GE' TO STATUS-WS                                         
270900           MOVE NEJ  TO INDATA-SW                                         
271000         END-IF                                                           
271100       END-IF                                                             
271200     END-PERFORM                                                          
271300     .                                                                    
271400     EJECT                                                                
271500 S09-LYS-UPP-INMATADE-FAELT SECTION.                                      
271600                                                                          
271700     IF (MID-BEART-IN = ALL '+' OR SPACE)                                 
271800       CONTINUE                                                           
271900     ELSE                                                                 
272000       MOVE NEJ TO INDATA-SW                                              
272100       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                       
272200     END-IF                                                               
272300                                                                          
272400     IF (MID-KDBENHOM-IN = ALL '+')                                       
272500       CONTINUE                                                           
272600     ELSE                                                                 
272700       MOVE NEJ TO INDATA-SW                                              
272800       MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                     
272900     END-IF                                                               
273000                                                                          
273100     IF (MID-BORT = ALL '+' OR SPACE)                                     
273200       CONTINUE                                                           
273300     ELSE                                                                 
273400       MOVE NEJ TO INDATA-SW                                              
273500       MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                           
273600     END-IF                                                               
273700                                                                          
273800     IF (MID-KDPRODSL-IN = ALL '+')                                       
273900       CONTINUE                                                           
274000     ELSE                                                                 
274100       MOVE NEJ TO INDATA-SW                                              
274200       MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                     
274300     END-IF                                                               
274400                                                                          
274500     IF (MID-IDFKNGRP-IN = ALL '+')                                       
274600       CONTINUE                                                           
274700     ELSE                                                                 
274800       MOVE NEJ TO INDATA-SW                                              
274900       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                     
275000     END-IF                                                               
275100                                                                          
275200     IF (MID-IDSTRTYP-IN = ALL '+' OR SPACE)                              
275300       CONTINUE                                                           
275400     ELSE                                                                 
275500       MOVE NEJ TO INDATA-SW                                              
275600       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                    
275700     END-IF                                                               
275800                                                                          
275900     IF (MID-TESTRNOT(1) = ALL '+' OR SPACE)                              
276000       CONTINUE                                                           
276100     ELSE                                                                 
276200       MOVE NEJ TO INDATA-SW                                              
276300       MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(1)                    
276400     END-IF                                                               
276500                                                                          
276600     IF (MID-TESTRNOT(2) = ALL '+' OR SPACE)                              
276700       CONTINUE                                                           
276800     ELSE                                                                 
276900       MOVE NEJ TO INDATA-SW                                              
277000       MOVE MFS-ALFA-FAELT-FEL TO MOD-TESTRNOT-ATTR(2)                    
277100     END-IF                                                               
277200     .                                                                    
277300     EJECT                                                                
277400 MFS-LAES-IN-IGEN SECTION.                                                
277500                                                                          
277600     IF UPDATE-NYUPPL-KOPIERING                                           
277700****** OM KOPIERING SKALL INTE DET REDAN SATTA ATTRIBUTET                 
277800****** (MFS-NUM-FAELT-FEL) LÄGGAS ÖVER                                    
277900       CONTINUE                                                           
278000     ELSE                                                                 
278100       IF (MID-IDARTNR-NY = ALL '+')                                      
278200         CONTINUE                                                         
278300       ELSE                                                               
278400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-NY-ATTR                
278500       END-IF                                                             
278600     END-IF                                                               
278700                                                                          
278800     IF (MID-BEART-IN = ALL '+')                                          
278900       CONTINUE                                                           
279000     ELSE                                                                 
279100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEART-IN-ATTR                    
279200     END-IF                                                               
279300                                                                          
279400     IF (MID-KDBENHOM-IN = ALL '+')                                       
279500       CONTINUE                                                           
279600     ELSE                                                                 
279700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-IN-ATTR                 
279800     END-IF                                                               
279900                                                                          
280000     IF (MID-BORT = ALL '+')                                              
280100       CONTINUE                                                           
280200     ELSE                                                                 
280300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BORT-ATTR                        
280400     END-IF                                                               
280500                                                                          
280600     IF (MID-KDPRODSL-IN = ALL '+')                                       
280700       CONTINUE                                                           
280800     ELSE                                                                 
280900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-IN-ATTR                 
281000     END-IF                                                               
281100                                                                          
281200     IF (MID-IDFKNGRP-IN = ALL '+')                                       
281300       CONTINUE                                                           
281400     ELSE                                                                 
281500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-IN-ATTR                 
281600     END-IF                                                               
281700                                                                          
281800     IF (MID-IDSTRTYP-IN = ALL '+')                                       
281900       CONTINUE                                                           
282000     ELSE                                                                 
282100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSTRTYP-IN-ATTR                 
282200     END-IF                                                               
282300                                                                          
282400     IF (MID-TESTRNOT(1) = ALL '+')                                       
282500       CONTINUE                                                           
282600     ELSE                                                                 
282700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TESTRNOT-ATTR(1)                 
282800     END-IF                                                               
282900                                                                          
283000     IF (MID-TESTRNOT(2) = ALL '+')                                       
283100       CONTINUE                                                           
283200     ELSE                                                                 
283300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TESTRNOT-ATTR(2)                 
283400     END-IF                                                               
283500                                                                          
283600     .                                                                    
283700     EJECT                                                                
283800 MFS-RENSA-FAELT-UT SECTION.                                              
283900                                                                          
284000     MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                                 
284100                             MOD-KDBENHOM-UT                              
284200                             MOD-TIREGDAT                                 
284300                             MOD-KDPRODSL-UT                              
284400                             MOD-IDFKNGRP-UT                              
284500                             MOD-IDSTRTYP-UT                              
284600                             MOD-IDLEVNR                                  
284700                             MOD-BELEVART                                 
284800                             MOD-KDERS                                    
284900                             MOD-TEARTNOT                                 
285000                             MOD-TEARTNOT-7                               
285100                                                                          
285200     MOVE +1 TO INDX                                                      
285300     PERFORM UNTIL INDX > MAX-IDAO                                        
285400       MOVE MFS-RENSA-FAELT TO MOD-IDAO(INDX)                             
285500       ADD +1 TO INDX                                                     
285600     END-PERFORM                                                          
285700     .                                                                    
285800     SKIP2                                                                
285900 MFS-RENSA-FAELT-IN SECTION.                                              
286000                                                                          
286100     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-NY                             
286200                               MOD-BEART-IN                               
286300                               MOD-KDBENHOM-IN                            
286400                               MOD-KDPRODSL-IN                            
286500                               MOD-IDFKNGRP-IN                            
286600                               MOD-IDSTRTYP-IN                            
286700                               MOD-BORT                                   
286800     .                                                                    
286900     SKIP2                                                                
287000 MFS-RENSA-FAELT-BADE-IN-O-UT SECTION.                                    
287100* RENSNING AV FÄLT SOM ÄR BÅDE IN- OCH UTMATNINGSFÄLT                     
287200                                                                          
287300     MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT(1)                              
287400                             MOD-TESTRNOT(2)                              
287500     .                                                                    
287600     EJECT                                                                
287700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
287800     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-UT                               
287900                               MOD-KDBENHOM-UT                            
288000                               MOD-TIREGDAT                               
288100                               MOD-KDPRODSL-UT                            
288200                               MOD-IDFKNGRP-UT                            
288300                               MOD-IDSTRTYP-UT                            
288400                               MOD-IDLEVNR                                
288500                               MOD-BELEVART                               
288600                               MOD-KDERS                                  
288700                               MOD-TEARTNOT                               
288800                               MOD-TEARTNOT-7                             
288900     MOVE +1 TO INDX                                                      
289000     PERFORM UNTIL INDX > MAX-IDAO                                        
289100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAO(INDX)                           
289200       ADD +1 TO INDX                                                     
289300     END-PERFORM                                                          
289400     .                                                                    
289500     SKIP2                                                                
289600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
289700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-NY                             
289800                               MOD-BEART-IN                               
289900                               MOD-KDBENHOM-IN                            
290000                               MOD-KDPRODSL-IN                            
290100                               MOD-IDFKNGRP-IN                            
290200                               MOD-IDSTRTYP-IN                            
290300                               MOD-BORT                                   
290400                               MOD-TESTRNOT(1)                            
290500                               MOD-TESTRNOT(2)                            
290600     .                                                                    
290700     SKIP2                                                                
290800 MFS-ROER-EJ-FAELT-BADE-IN-O-UT SECTION.                                  
290900                                                                          
291000     MOVE MFS-ROER-EJ-FAELT TO MOD-TESTRNOT(1)                            
291100                               MOD-TESTRNOT(2)                            
291200     .                                                                    
291300     EJECT                                                                
291400 MFS-STAENG-FAELT-IN  SECTION.                                            
291500     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDARTNR-NY-ATTR                   
291600                                    MOD-BEART-IN-ATTR                     
291700                                    MOD-KDBENHOM-IN-ATTR                  
291800                                    MOD-KDPRODSL-IN-ATTR                  
291900                                    MOD-IDFKNGRP-IN-ATTR                  
292000                                    MOD-IDSTRTYP-IN-ATTR                  
292100                                    MOD-BORT-ATTR                         
292200                                    MOD-TESTRNOT-ATTR(1)                  
292300                                    MOD-TESTRNOT-ATTR(2)                  
292400     .                                                                    
292500     SKIP2                                                                
292600 MFS-FORM-ATTR SECTION.                                                   
292700                                                                          
292800     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-NY-ATTR                       
292900                                MOD-BEART-IN-ATTR                         
293000                                MOD-KDBENHOM-IN-ATTR                      
293100                                MOD-KDPRODSL-IN-ATTR                      
293200                                MOD-IDFKNGRP-IN-ATTR                      
293300                                MOD-IDSTRTYP-IN-ATTR                      
293400                                MOD-BORT-ATTR                             
293500                                MOD-TESTRNOT-ATTR(1)                      
293600                                MOD-TESTRNOT-ATTR(2)                      
293700     .                                                                    
293800     EJECT                                                                
293900* IMS SEKTIONER                                                           
294000     SKIP3                                                                
294100 IMS-GET-MSG SECTION.                                                     
294200                                                                          
294300     MOVE '  QC' TO GODK-STATUSKODER                                      
294400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
294500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
294600     PERFORM IMS-STATUSKONTROLL                                           
294700     .                                                                    
294800     SKIP3                                                                
294900 IMS-INSERT-MSG SECTION.                                                  
295000                                                                          
295100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
295200       MOVE '0' TO MFS-KDHUVOMR                                           
295300     END-IF                                                               
295400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
295500     MOVE SPACE TO GODK-STATUSKODER                                       
295600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
295700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
295800     PERFORM IMS-STATUSKONTROLL                                           
295900     .                                                                    
296000     EJECT                                                                
296100 IMS-GET-ARTC01 SECTION.                                                  
296200                                                                          
296300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
296400          DELIMITED BY SIZE INTO SSA1                                     
296500     MOVE '  GE' TO GODK-STATUSKODER                                      
296600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
296700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
296800     PERFORM IMS-STATUSKONTROLL                                           
296900     .                                                                    
297000     SKIP3                                                                
297100 IMS-GET-ARTC25 SECTION.                                                  
297200                                                                          
297300     MOVE 'WLARTC11 ' TO SSA1                                             
297400     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
297500          DELIMITED BY SIZE INTO SSA2                                     
297600     MOVE '  GE' TO GODK-STATUSKODER                                      
297700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
297800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
297900     PERFORM IMS-STATUSKONTROLL                                           
298000     .                                                                    
298100     SKIP3                                                                
298200 IMS-GET-ARTC11 SECTION.                                                  
298300                                                                          
298400     MOVE 'WLARTC11 ' TO SSA1                                             
298500     MOVE '  GE' TO GODK-STATUSKODER                                      
298600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
298700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
298800     PERFORM IMS-STATUSKONTROLL                                           
298900     .                                                                    
299000     EJECT                                                                
299100 IMS-GET-BENA01-ASEQ SECTION.                                             
299200                                                                          
299300     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
299400                                  W-BEART-X ')'                           
299500          DELIMITED BY SIZE INTO SSA1                                     
299600     MOVE '  GE' TO GODK-STATUSKODER                                      
299700     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
299800     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
299900     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
300100     SKIP3                                                                
300200 IMS-GET-BENA01-ASEQ-NEXT SECTION.                                        
300300                                                                          
300400     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
300500                                  W-BEART-X ')'                           
300600          DELIMITED BY SIZE INTO SSA1                                     
300700     MOVE '  GE' TO GODK-STATUSKODER                                      
300800     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
300900     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
301000     PERFORM IMS-STATUSKONTROLL                                           
301100     .                                                                    
301200     SKIP3                                                                
301300 IMS-GET-BENA11-ASEQ SECTION.                                             
301400                                                                          
301500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
301600          DELIMITED BY SIZE INTO SSA1                                     
301700     MOVE '  GE' TO GODK-STATUSKODER                                      
301800     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
301900     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
302000     PERFORM IMS-STATUSKONTROLL                                           
302100     .                                                                    
302200     EJECT                                                                
302300 IMS-GET-BENA01-BSEQ SECTION.                                             
302400                                                                          
302500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
302600          DELIMITED BY SIZE INTO SSA1                                     
302700     MOVE '  GE' TO GODK-STATUSKODER                                      
302800     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
302900     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
303000     PERFORM IMS-STATUSKONTROLL                                           
303100     .                                                                    
303200     SKIP3                                                                
303300 IMS-GET-BENA11-BSEQ SECTION.                                             
303400                                                                          
303500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
303600          DELIMITED BY SIZE INTO SSA1                                     
303700     MOVE '  GE' TO GODK-STATUSKODER                                      
303800     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
303900     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
304000     PERFORM IMS-STATUSKONTROLL                                           
304100     .                                                                    
304200     EJECT                                                                
304300 IMS-GET-WDF501 SECTION.                                                  
304400                                                                          
304500     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
304600          DELIMITED BY SIZE INTO SSA1                                     
304700     MOVE '  GE' TO GODK-STATUSKODER                                      
304800     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
304900     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
305000     PERFORM IMS-STATUSKONTROLL                                           
305100     .                                                                    
305200     SKIP3                                                                
305300 IMS-GET-WDF502-LAST SECTION.                                             
305400                                                                          
305500     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
305600          DELIMITED BY SIZE INTO SSA1                                     
305700     MOVE '  GE' TO GODK-STATUSKODER                                      
305800     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
305900     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
306000     PERFORM IMS-STATUSKONTROLL                                           
306100     .                                                                    
306200     SKIP3                                                                
306300 IMS-GET-WDF502-OKVAL SECTION.                                            
306400                                                                          
306500     MOVE 'WDF502  ' TO SSA1                                              
306600     MOVE '  GE' TO GODK-STATUSKODER                                      
306700     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
306800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
306900     PERFORM IMS-STATUSKONTROLL                                           
307000     .                                                                    
307100     EJECT                                                                
307200 IMS-DLET-SATB SECTION.                                                   
307300                                                                          
307400     MOVE '  ' TO GODK-STATUSKODER                                        
307500     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
307600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
307700     PERFORM IMS-STATUSKONTROLL                                           
307800     .                                                                    
307900     SKIP3                                                                
308000 IMS-GET-SATB01 SECTION.                                                  
308100                                                                          
308200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
308300          DELIMITED BY SIZE INTO SSA1                                     
308400     MOVE '  GE' TO GODK-STATUSKODER                                      
308500     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
308600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
308700     PERFORM IMS-STATUSKONTROLL                                           
308800     .                                                                    
308900     SKIP3                                                                
309000 IMS-GET-SATB11 SECTION.                                                  
309100                                                                          
309200     MOVE 'WLSATB11 ' TO SSA1                                             
309300     MOVE '  GE' TO GODK-STATUSKODER                                      
309400     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
309500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
309600     PERFORM IMS-STATUSKONTROLL                                           
309700     .                                                                    
309800     SKIP3                                                                
309900 IMS-GET-SATB22 SECTION.                                                  
310000                                                                          
310100     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
310200                                  W-IDRADNR-X ')'                         
310300          DELIMITED BY SIZE INTO SSA1                                     
310400     MOVE 'WLSATB22 ' TO SSA2                                             
310500     MOVE '  GE' TO GODK-STATUSKODER                                      
310600     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
310700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
310800     PERFORM IMS-STATUSKONTROLL                                           
310900     .                                                                    
311000     EJECT                                                                
311100 IMS-ISRT-SATB01 SECTION.                                                 
311200                                                                          
311300     MOVE 'WLSATB01' TO SSA1                                              
311400     MOVE '  ' TO GODK-STATUSKODER                                        
311500     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
311600     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
311700     PERFORM IMS-STATUSKONTROLL                                           
311800     .                                                                    
311900     SKIP3                                                                
312000 IMS-ISRT-SATB11 SECTION.                                                 
312100                                                                          
312200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-I-X ')'                       
312300          DELIMITED BY SIZE INTO SSA1                                     
312400     MOVE 'WLSATB11 ' TO SSA2                                             
312500     MOVE '  ' TO GODK-STATUSKODER                                        
312600     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
312700                                                     SSA2                 
312800     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
312900     PERFORM IMS-STATUSKONTROLL                                           
313000     .                                                                    
313100     SKIP3                                                                
313200 IMS-ISRT-SATB22 SECTION.                                                 
313300                                                                          
313400     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-I-X ')'                       
313500          DELIMITED BY SIZE INTO SSA1                                     
313600     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
313700                                  W-IDRADNR-X ')'                         
313800          DELIMITED BY SIZE INTO SSA2                                     
313900     MOVE 'WLSATB22 ' TO SSA3                                             
314000     MOVE '  ' TO GODK-STATUSKODER                                        
314100     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
314200                                                     SSA2                 
314300                                                     SSA3                 
314400     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
314500     PERFORM IMS-STATUSKONTROLL                                           
314600     .                                                                    
314700     EJECT                                                                
314800 IMS-REPL-SATB SECTION.                                                   
314900                                                                          
315000     MOVE '  ' TO GODK-STATUSKODER                                        
315100     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
315200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
315300     PERFORM IMS-STATUSKONTROLL                                           
315400     .                                                                    
315500     EJECT                                                                
315600 IMS-GET-SATB-CSEQ-OKONV-UNIK SECTION.                                    
315700* OBS! ENDAST EJ KONVERTERADE STRUKTURER 'TAS IN'                         
315800                                                                          
315900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
316000                                    W-BELEVART-X                          
316100                                    W-IDARTNR-X ')'                       
316200          DELIMITED BY SIZE INTO SSA1                                     
316300     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-MIN-X                         
316400                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
316500          DELIMITED BY SIZE INTO SSA2                                     
316600     MOVE '  GE' TO GODK-STATUSKODER                                      
316700     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA SSA1 SSA2               
316800     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
316900     PERFORM IMS-STATUSKONTROLL                                           
317000     .                                                                    
317100     SKIP3                                                                
317200 IMS-GET-SATB-CSEQ-OKONV-NEXT SECTION.                                    
317300* OBS! ENDAST EJ KONVERTERADE STRUKTURER 'TAS IN'                         
317400                                                                          
317500     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
317600                                    W-BELEVART-X                          
317700                                    W-IDARTNR-X ')'                       
317800          DELIMITED BY SIZE INTO SSA1                                     
317900     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-MIN-X                         
318000                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
318100          DELIMITED BY SIZE INTO SSA2                                     
318200     MOVE '  GE' TO GODK-STATUSKODER                                      
318300     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA SSA1 SSA2               
318400     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
318500     PERFORM IMS-STATUSKONTROLL                                           
318600     .                                                                    
318700     SKIP3                                                                
318800 IMS-GET-SATB-CSEQ-KONV-UNIK SECTION.                                     
318900* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
319000                                                                          
319100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
319200                                    W-BELEVART-X                          
319300                                    W-IDARTNR-X ')'                       
319400          DELIMITED BY SIZE INTO SSA1                                     
319500     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-KONV-MIN-X                    
319600                    '&IDARTNR <=' W-IDARTNR-KONV-MAX-X ')'                
319700          DELIMITED BY SIZE INTO SSA2                                     
319800     MOVE '  GE' TO GODK-STATUSKODER                                      
319900     CALL CBLTDLI USING GHU SATB-C-PCB DLI-IO-AREA SSA1 SSA2              
320000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
320100     PERFORM IMS-STATUSKONTROLL                                           
320200     .                                                                    
320300     SKIP3                                                                
320400 IMS-GET-SATB-CSEQ-KONV-NEXT SECTION.                                     
320500* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
320600                                                                          
320700     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
320800                                    W-BELEVART-X                          
320900                                    W-IDARTNR-X ')'                       
321000          DELIMITED BY SIZE INTO SSA1                                     
321100     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-KONV-MIN-X                    
321200                    '&IDARTNR <=' W-IDARTNR-KONV-MAX-X ')'                
321300          DELIMITED BY SIZE INTO SSA2                                     
321400     MOVE '  GE' TO GODK-STATUSKODER                                      
321500     CALL CBLTDLI USING GHN SATB-C-PCB DLI-IO-AREA SSA1 SSA2              
321600     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
321700     PERFORM IMS-STATUSKONTROLL                                           
321800     .                                                                    
321900     EJECT                                                                
322000 IMS-GET-SATB01-DSEQ SECTION.                                             
322100* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
322200                                                                          
322300     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
322400                                  W-IDARTNR-KONV-MIN-X                    
322500                    '&WDJ1DSEQ<=' W-IDUSER-X                              
322600                                  W-IDARTNR-KONV-MAX-X ')'                
322700          DELIMITED BY SIZE INTO SSA1                                     
322800     MOVE '  GE' TO GODK-STATUSKODER                                      
322900     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
323000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
323100     PERFORM IMS-STATUSKONTROLL                                           
323200     .                                                                    
323300     EJECT                                                                
323400 IMS-GET-XXAZ01 SECTION.                                                  
323500                                                                          
323600     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
323700          DELIMITED BY SIZE INTO SSA1                                     
323800     MOVE '    ' TO GODK-STATUSKODER                                      
323900     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA SSA1                      
324000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
324100     PERFORM IMS-STATUSKONTROLL                                           
324200     .                                                                    
324300     SKIP3                                                                
324400 IMS-DLET-XXAZ11 SECTION.                                                 
324500                                                                          
324600     MOVE '  ' TO GODK-STATUSKODER                                        
324700     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA                         
324800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
324900     PERFORM IMS-STATUSKONTROLL                                           
325000     .                                                                    
325100     SKIP3                                                                
325200 IMS-GET-XXAZ11-IDUSER SECTION.                                           
325300                                                                          
325400     STRING 'WLXXAZ11(IDUSER   =' W-IDUSER-X ')'                          
325500          DELIMITED BY SIZE INTO SSA1                                     
325600     MOVE '  GE' TO GODK-STATUSKODER                                      
325700     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
325800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
325900     PERFORM IMS-STATUSKONTROLL                                           
326000     .                                                                    
326100     EJECT                                                                
326200 IMS-STATUSKONTROLL SECTION.                                              
326300                                                                          
326400     SET STATUS-IX TO 1                                                   
326500     SEARCH GODK-STATUS                                                   
326600       AT END CALL FELLOG                                                 
326700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
326800     END-SEARCH                                                           
326900     .                                                                    
327000     EJECT                                                                
327100     EJECT                                                                
327200*    -COPY WY2000P1                                                       
