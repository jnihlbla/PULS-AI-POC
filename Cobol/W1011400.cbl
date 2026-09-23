000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1011400.                                                
000400 AUTHOR.         JANNE MELANDER                                           
000500 DATE-WRITTEN.   MARS. 88.                                                
000600                                                                          
000700***************************************************************           
000800*  FUNKTION. -  REGISTRERING AV ARTIKEL TILL BASLAGERMARKNAD. *           
000900*                                                             *           
001000*            -  KOPIERING AV ARTIKEL OCH DESS BASLAGER-       *           
001100*               MARKNADSSEGMENT TILL NY ARTIKEL.              *           
001200***************************************************************           
001300*  ÄNDRING.  -  LÄSNING AV MARKNADSSPECIFIKA TIPROJSTO OCH    *           
001400*  AUG -91      TISTAMREG FRÅN PROJ-BASEN VID UPPDATERING AV  *           
001500*               TISTOMREG PÅ WLARTG11.                        *           
001600***************************************************************           
001700*  ÄNDRING.  -  FLBLMQ  TILLAGT WLARTG11                      *           
001800*  APRIL -92    - SÄTTS TILL NEJ VID NYUPPLÄGG                *           
001900*               - SÄTTS TILL JA NÄR ARTIKEL TAS BORT FRÅN     *           
002000*                 MARKNADENS KÖ ELLER TVINGAS UPP IGEN        *           
002100*                 (NY INFORMATION FINNS)                      *           
002200***************************************************************           
002300     NYCKLAR:     IDARTNR                                                 
002400                                                                          
002500     TRANSAKTION: W1T114                                                  
002600                                                                          
002700     INDATA:      MID:      W1I11401                                      
002800                  MID:      W1I16101                                      
002900                                                                          
003000     UTDATA.      MOD:      W1O11401                                      
003100                                                                          
003200     EJECT                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900     SKIP3                                                                
004000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W1011400'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  FORTS-COPY                  PIC X       VALUE SPACE.                 
004400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  W-ANT-DIST                  PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  REBL-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  BAS-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  COL-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  KOP-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77  DIST-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77  DIST-KOLL                   PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77  MARK-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77  MAX-IX-PLUS-1               PIC S9(9)   VALUE +15  COMP SYNC.        
005600 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +900 COMP SYNC.        
005700                                                                          
005800 77  WS1-KDPRODSL                PIC 9(2)    VALUE ZERO.                  
005900 77  WS-KDPRODSL                 PIC 9(2)    VALUE ZERO.                  
006000 77  WS-IDAO                     PIC X(10)   VALUE SPACE.                 
006100 77  WS-TEARTNOT-BASL            PIC X(40)   VALUE SPACE.                 
006200 77  WS-TEARTNOT-VARNOT          PIC X(40)   VALUE SPACE.                 
006300 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
006400 77  WS-FLBASL                   PIC X       VALUE SPACE.                 
006500 77  WS-IDARTNR-ERS              PIC 9(9)    VALUE ZERO.                  
006600 77  WS-KDBPSR                   PIC 9(1)    VALUE ZERO.                  
006700 77  WS-IDFKNGRP                 PIC 9(5)    VALUE ZERO.                  
006800 77  WS-IDARTNR                  PIC X(9)    VALUE ZERO.                  
006900 77  BAS-AER-LIKA-MED            PIC 9(1)    VALUE ZERO.                  
007000 77  WS-TIBASL                   PIC 9(7)    VALUE ZERO.                  
007100 77  WS-DATUM1                   PIC 9(7)    VALUE ZERO.                  
007200 77  WS-DATUM2                   PIC 9(8).                                
007300 77  WS-GENERELL-TISTAMREG       PIC 9(7)    VALUE ZERO.                  
007400 77  WS-GENERELL-TIPROJSTO       PIC 9(7)    VALUE ZERO.                  
007500 77  WS-MARKNAD-TISTAMREG        PIC 9(7)    VALUE ZERO.                  
007600 77  WS-MARKNAD-TIPROJSTO        PIC 9(7)    VALUE ZERO.                  
007700 77  WS-TA-BORT-ANT              PIC 9(7)    VALUE ZERO.                  
007800 77  WS-KVBASLM                  PIC 9(7)    VALUE ZERO.                  
007900                                                                          
008000 77  INDATA-SW                   PIC X.                                   
008100   88  INDATA-OK                             VALUE 'J'.                   
008200     SKIP3                                                                
008300 77  UPPDATERING-SW             PIC X.                                    
008400   88  UPPDATERING-OK                        VALUE 'J'.                   
008500     SKIP3                                                                
008600 77  RADUPPDAT-SW               PIC X.                                    
008700   88  RADUPPDAT-OK                          VALUE 'J'.                   
008800     SKIP3                                                                
008900 77  TIDUPPDAT-SW               PIC X.                                    
009000   88  TIDUPPDAT-OK                          VALUE 'J'.                   
009100     SKIP3                                                                
009200*01  -COPY WWPRODSL                                                       
009300                                                                          
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDCKONS                                                     
009700*01    -COPY WWDC99                                                       
009800       EJECT                                                              
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010300                                                                          
010400 01  DAGENS-DATUM                PIC 9(6) VALUE ZERO.                     
010500 01  VECKOR.                                                              
010600     03  AAVVD                   PIC 9(5).                                
010700     03  FILLER REDEFINES AAVVD.                                          
010800         05  AAVV                PIC 9(4).                                
010900         05  D                   PIC 9(1).                                
011000 01  MANDER.                                                              
011100     03  AAMMDD                  PIC 9(6).                                
011200     03  FILLER REDEFINES AAMMDD.                                         
011300         05  AAMM                PIC 9(4).                                
011400         05  DD                  PIC 9(2).                                
011500                                                                          
011600 01  FELKDBASLM1126.                                                      
011700     03  WFELKDBASLM             PIC X(40).                               
011800     03  FILLER REDEFINES WFELKDBASLM.                                    
011900         05  WFELMARKN           PIC X(6).                                
012000         05  WFELTEXT            PIC X(34).                               
012100 01  NYCKLAR-TILL-DLI.                                                    
012200                                                                          
012300   03  W-1123-KEY-X.                                                      
012400     05  FILLER                  PIC X(4)    VALUE '1123'.                
012500     05  W-1123-KDPRODSL         PIC S9(3)   COMP-3 VALUE ZERO.           
012600     05  W-1123-IDPROJ           PIC X(4)    VALUE SPACE.                 
012700     05  FILLER                  PIC X(20)   VALUE LOW-VALUE.             
012800                                                                          
012900   03  W-1124-KEY-X.                                                      
013000     05  W-1124-KDSEGKEY         PIC X(1)    VALUE '1'.                   
013100                                                                          
013200   03  W-KDNOTTYP-X.                                                      
013300     05  W-KDNOTTYP              PIC S9      COMP-3.                      
013400                                                                          
013500   03  W-1126-KEY-X.                                                      
013600     05  W-1126-KDBASLM          PIC X(6)    VALUE SPACE.                 
013700     05  FILLER                  PIC X(9)    VALUE LOW-VALUE.             
013800                                                                          
013900   03  W-IDARTNR-X.                                                       
014000     05  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
014100                                                                          
014200   03  WX-IDARTNR-X.                                                      
014300     05  WX-IDARTNR              PIC S9(9)   COMP-3  VALUE ZERO.          
014400                                                                          
014500   03  W-IDSKYLT-X.                                                       
014600     05  W-IDSKYLT               PIC X(3)    VALUE 'S  '.                 
014700                                                                          
014800   03  W-KDBASLM-X.                                                       
014900     05  W-KDBASLM               PIC  X(6)   VALUE SPACE.                 
015000                                                                          
015100   03  W-WDD2C1KY-MIN.                                                    
015200     05  W-IDAO-MIN               PIC X(10).                              
015300     05  W-IDARTNR-MIN            PIC S9(9)                               
015400                                        COMP-3 VALUE ZERO.                
015500                                                                          
015600   03  W-WDD2C1KY-MAX.                                                    
015700     05  W-IDAO-MAX               PIC X(10).                              
015800     05  FILLER                   PIC S9(9)                               
015900                                      COMP-3 VALUE +999999999.            
016000                                                                          
016100   03  W-IDPROJ-X.                                                        
016200     05  W-IDPROJ                 PIC X(4) VALUE SPACE.                   
016300                                                                          
016400   03  W-WDD7A1KY-MIN.                                                    
016500     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
016600     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
016700     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
016800                                                                          
016900   03  W-WDD7A1KY-MAX.                                                    
017000     05  W-IDARTNR-MAX     PIC S9(9)  COMP-3 VALUE ZERO.                  
017100     05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
017200     05  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
017300                                                                          
017400                                                                          
017500     EJECT                                                                
017600*** VARIABLER TILL W009VADD ****************************                  
017700                                                                          
017800 01  W009VADD-DATUM              PIC S9(5)  COMP-3.                       
017900 01  W009VADD-ANTAL              PIC S9(5)  COMP-3.                       
018000                                                                          
018100 01  DYNAMISKA-SUBPROGRAM.                                                
018200     03  WORKDAY                 PIC X(8) VALUE 'WORKDAY '.               
018300                                                                          
018400*01  -COPY WORKAREA                                                       
018500     EJECT                                                                
018600     03  WMSGINIT                PIC X(8) VALUE 'WMSGINIT'.               
018700*                   ****   PARAMETRAR TILL W005INIT                       
018800*01  -COPY WMSGINIT                                                       
018900     EJECT                                                                
019000*** UPPLYSNINGSTEXT ************************************                  
019100                                                                          
019200 01  MEDDELANDE.                                                          
019300   03  FEL1.                                                              
019400     05 FILLER                   PIC X(40)                                
019500          VALUE 'UPPLYSTA FÄLT FEL'.                                      
019600     05 FILLER                   PIC X(40)                                
019700          VALUE 'CORRECT HIGHLIGHTED FIELDS'.                             
019800   03  FILLER REDEFINES FEL1.                                             
019900     05  FEL-1                   PIC X(40)   OCCURS 2.                    
020000                                                                          
020100   03  FEL2.                                                              
020200     05 FILLER                   PIC X(40)                                
020300          VALUE 'PROJEKTSTRUKTUR  SAKNAS I BASEN'.                        
020400     05 FILLER                   PIC X(40)                                
020500          VALUE 'PROJECT STRUCTURE IS MISSING  '.                         
020600   03  FILLER REDEFINES FEL2.                                             
020700     05  FEL-2                   PIC X(40)   OCCURS 2.                    
020800                                                                          
020900   03  FEL3.                                                              
021000     05 FILLER                   PIC X(40)                                
021100          VALUE 'MARKNADER SAKNAS FÖR KOPIERING'.                         
021200     05 FILLER                   PIC X(40)                                
021300          VALUE 'MARKETS MISSING              '.                          
021400   03  FILLER REDEFINES FEL3.                                             
021500     05  FEL-3                   PIC X(40)   OCCURS 2.                    
021600                                                                          
021700   03  FEL4.                                                              
021800     05 FILLER                   PIC X(40)                                
021900          VALUE 'ANGIVEN MARKNAD FINNS REDAN   '.                         
022000     05 FILLER                   PIC X(40)                                
022100          VALUE 'MARKET ALREADY EXISTS  '.                                
022200   03  FILLER REDEFINES FEL4.                                             
022300     05  FEL-4                   PIC X(40)   OCCURS 2.                    
022400                                                                          
022500   03  FEL5.                                                              
022600     05 FILLER                   PIC X(40)                                
022700          VALUE 'UPPDATERING INTE TILLÅTEN     '.                         
022800     05 FILLER                   PIC X(40)                                
022900          VALUE 'UPDATE NOT ALLOWED      '.                               
023000   03  FILLER REDEFINES FEL5.                                             
023100     05  FEL-5                   PIC X(40)   OCCURS 2.                    
023200                                                                          
023300   03  FEL6.                                                              
023400     05 FILLER                   PIC X(40)                                
023500          VALUE 'PRODSLNR FELAKTIGT.  EJ GILTIGT VCC/VCBV'.               
023600     05 FILLER                   PIC X(40)                                
023700          VALUE 'PRODUCT CODE WRONG. NOT A VALID VCC/VCBV'.               
023800   03  FILLER REDEFINES FEL6.                                             
023900     05  FEL-6                   PIC X(40)   OCCURS 2.                    
024000                                                                          
024100   03  FEL7.                                                              
024200     05 FILLER                   PIC X(40)                                
024300          VALUE 'KOP. EJ TILLÅTEN.    MARKNAD FINNS REDAN'.               
024400     05 FILLER                   PIC X(40)                                
024500          VALUE 'COPY NOT ALLOWED.  MARKET ALREADY EXISTS'.               
024600   03  FILLER REDEFINES FEL7.                                             
024700     05  FEL-7                   PIC X(40)   OCCURS 2.                    
024800                                                                          
024900   03  FEL8.                                                              
025000     05 FILLER                   PIC X(40)                                
025100          VALUE 'GÅR EJ KOP. NY ART SAKNAR PROJSTRUKTUR  '.               
025200     05 FILLER                   PIC X(40)                                
025300          VALUE 'COPY FAILURE. NEW PART LACK PROJ STRUCT.'.               
025400   03  FILLER REDEFINES FEL8.                                             
025500     05  FEL-8                   PIC X(40)   OCCURS 2.                    
025600                                                                          
025700   03  FEL9.                                                              
025800     05 FILLER                   PIC X(40)                                
025900          VALUE 'NO.QTY=J   ART INTE UPP PÅ MARKNADS KÖN '.               
026000     05 FILLER                   PIC X(40)                                
026100          VALUE 'NO.QTY=J   PART NOT PUT ON MARKET QUEUE '.               
026200   03  FILLER REDEFINES FEL9.                                             
026300     05  FEL-9                   PIC X(40)   OCCURS 2.                    
026400                                                                          
026500   03  FEL10.                                                             
026600     05 FILLER                   PIC X(40)                                
026700          VALUE 'ARTIKELN SAKNAS                     '.                   
026800     05 FILLER                   PIC X(40)                                
026900          VALUE 'PART IS MISSING                     '.                   
027000   03  FILLER REDEFINES FEL10.                                            
027100     05  FEL-10                  PIC X(40)   OCCURS 2.                    
027200                                                                          
027300   03  MED1.                                                              
027400     05 FILLER                   PIC X(40)                                
027500          VALUE 'UPPDATERING GJORD        '.                              
027600     05 FILLER                   PIC X(40)                                
027700          VALUE 'DATA BASE HAS BEEN UPDATED  '.                           
027800   03  FILLER REDEFINES MED1.                                             
027900     05  MED-1                   PIC X(40)   OCCURS 2.                    
028000                                                                          
028100   03  MED2.                                                              
028200     05 FILLER                   PIC X(40)                                
028300          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
028400     05 FILLER                   PIC X(40)                                
028500          VALUE 'PRESS PF8 FOR MORE LINES'.                               
028600   03  FILLER REDEFINES MED2.                                             
028700     05  MED-2                   PIC X(40)   OCCURS 2.                    
028800                                                                          
028900   03  MED3.                                                              
029000     05 FILLER                   PIC X(40)                                
029100          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
029200     05 FILLER                   PIC X(40)                                
029300          VALUE 'THIS IS THE FIRST PAGE'.                                 
029400   03  FILLER REDEFINES MED3.                                             
029500     05  MED-3                   PIC X(40)   OCCURS 2.                    
029600                                                                          
029700   03  MED4.                                                              
029800     05 FILLER                   PIC X(40)                                
029900          VALUE 'DETTA ÄR SISTA SIDAN'.                                   
030000     05 FILLER                   PIC X(40)                                
030100          VALUE 'THIS IS THE LAST PAGE'.                                  
030200   03  FILLER REDEFINES MED4.                                             
030300     05  MED-4                   PIC X(40)   OCCURS 2.                    
030400                                                                          
030500   03  MED5.                                                              
030600     05 FILLER                   PIC X(40)                                
030700          VALUE 'PASSIVMÄRKT ARTIKEL '.                                   
030800     05 FILLER                   PIC X(40)                                
030900          VALUE 'PART IS MARKED PASSIVE '.                                
031000   03  FILLER REDEFINES MED5.                                             
031100     05  MED-5                   PIC X(40)   OCCURS 2.                    
031200                                                                          
031300   03  MED6.                                                              
031400     05 FILLER                   PIC X(40)                                
031500          VALUE 'MILITÄR ARTIKEL                        '.                
031600     05 FILLER                   PIC X(40)                                
031700          VALUE 'PART ASSIGNED TO MILITARY CATEGORY.    '.                
031800   03  FILLER REDEFINES MED6.                                             
031900     05  MED-6                   PIC X(40)   OCCURS 2.                    
032000                                                                          
032100   03  MED7.                                                              
032200     05 FILLER                   PIC X(40)                                
032300          VALUE 'SPECIAL ARTIKEL     '.                                   
032400     05 FILLER                   PIC X(40)                                
032500          VALUE 'PART ASSIGNED AS A SPECIAL PART '.                       
032600   03  FILLER REDEFINES MED7.                                             
032700     05  MED-7                   PIC X(40)   OCCURS 2.                    
032800                                                                          
032900   03  MED8.                                                              
033000     05 FILLER                   PIC X(40)                                
033100          VALUE 'MONTERINGSANVISNING '.                                   
033200     05 FILLER                   PIC X(40)                                
033300          VALUE 'ASSEMBLY DESCRIPTION            '.                       
033400   03  FILLER REDEFINES MED8.                                             
033500     05  MED-8                   PIC X(40)   OCCURS 2.                    
033600                                                                          
033700   03  MED9.                                                              
033800     05 FILLER                   PIC X(40)                                
033900          VALUE 'ARTIKELN SKICKAS TILL MARKN KÖ IGEN'.                    
034000     05 FILLER                   PIC X(40)                                
034100          VALUE 'PART IS SENT TO MARKET QUEUE AGAIN '.                    
034200   03  FILLER REDEFINES MED9.                                             
034300     05  MED-9                   PIC X(40)   OCCURS 2.                    
034400                                                                          
034500   03  MED10.                                                             
034600     05 FILLER                   PIC X(40)                                
034700          VALUE 'ARTIKELN RIVS NED FRÅN MARKNADS KÖN'.                    
034800     05 FILLER                   PIC X(40)                                
034900          VALUE 'PART IS TAKEN DOWN FROM MARKET QUEUE'.                   
035000   03  FILLER REDEFINES MED10.                                            
035100     05  MED-10                  PIC X(40)   OCCURS 2.                    
035200                                                                          
035300   03  MED11.                                                             
035400     05 FILLER                   PIC X(40)                                
035500          VALUE 'ALLA MARKN. STOPPDAT NYSÄTTS PÅ 9121'.                   
035600     05 FILLER                   PIC X(40)                                
035700          VALUE 'LAST-DATES ARE RESET ON ALL MARKETS   '.                 
035800   03  FILLER REDEFINES MED11.                                            
035900     05  MED-11                  PIC X(40)   OCCURS 2.                    
036000                                                                          
036100   03  MED12.                                                             
036200     05 FILLER                   PIC X(40)                                
036300          VALUE 'KOPIERING UTFÖRD. ARTIKEL TILL MARKN-KÖN'.               
036400     05 FILLER                   PIC X(40)                                
036500          VALUE 'COPY DONE, PART ASSIGNED TO MARKET QUEUE'.               
036600   03  FILLER REDEFINES MED12.                                            
036700     05  MED-12                  PIC X(40)   OCCURS 2.                    
036800                                                                          
036900     EJECT                                                                
037000******************************************************************        
037100*                                                                         
037200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
037300*                                                                         
037400 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
037500     SKIP3                                                                
037600*01  MID -COPY W1I11401                                                   
037700     EJECT                                                                
037800*01  MID -COPY W1I16101    -PRE 1161-                                     
037900     EJECT                                                                
038000*01  MID -COPY W9I12201    -PRE 9122-                                     
038100     EJECT                                                                
038200*01  -COPY WMSGAREA                                                       
038300     EJECT                                                                
038400*  03  MOD -COPY W1O11401           -RED MSG-AREA.                        
038500     EJECT                                                                
038600*01  -COPY WMFSAREA                                                       
038700     EJECT                                                                
038800******************************************************************        
038900*                                                                         
039000*  03  WLARTG11  -COPY WDD211    -PRE WS1-.                               
039100     EJECT                                                                
039200*  03  WLARTG11  -COPY WDD211    -PRE WORK-.                              
039300     EJECT                                                                
039400*  03  WLARTC25  -COPY WDK625    -PRE WORK-.                              
039500     EJECT                                                                
039600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039700*                                                                         
039800 01  IMS-WS.                                                              
039900   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
040000     SKIP3                                                                
040100*                        **** STATUS-KOD FRÅN IMS                         
040200   03  STATUS-WS                 PIC XX.                                  
040300     88  SEGMENT-FINNS                       VALUE '  '.                  
040400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040500     SKIP3                                                                
040600   03  GODK-STATUSKODER.                                                  
040700     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040800     SKIP3                                                                
040900 01    SSA1                      PIC X(96).                               
041000 01    SSA2                      PIC X(64).                               
041100 01    SSA3                      PIC X(64).                               
041200     EJECT                                                                
041300*                            IMS FUNKTIONSKODER                           
041400*01    -COPY W0003                                                        
041500     EJECT                                                                
041600*                            DLI INPUT-OUTPUT AREA                        
041700 01  DLI-IO-AREA1.                                                        
041800   03  IO-AREA1                 PIC X(50)  VALUE SPACE.                   
041900     SKIP3                                                                
042000*  03  WLXXAP01  -COPY WDGX1123            -RED IO-AREA1.                 
042100     EJECT                                                                
042200*  03  WLXXAP11  -COPY WDGX1124            -RED IO-AREA1.                 
042300     EJECT                                                                
042400 01  DLI-IO-AREA2.                                                        
042500   03  IO-AREA2                 PIC X(80)  VALUE SPACE.                   
042600*  03  WLXXAP12  -COPY WDGX1126            -RED IO-AREA2.                 
042700     EJECT                                                                
042800 01  DLI-IO-AREA3.                                                        
042900   03  IO-AREA3                 PIC X(550)  VALUE SPACE.                  
043000     SKIP3                                                                
043100*  03  ERSB01 -COPY WDD7A1    -PRE ERSB01- -RED IO-AREA3.                 
043200     EJECT                                                                
043300*  03  WLARTG01  -COPY WDD201 -PRE ARTG-   -RED IO-AREA3.                 
043400     EJECT                                                                
043500*  03  WLARTG11  -COPY WDD211 -PRE ARTG11- -RED IO-AREA3.                 
043600     EJECT                                                                
043700*  03  WLARTJ01  -COPY WDD2C1              -RED IO-AREA3.                 
043800     EJECT                                                                
043900*  03  WLBENA11  -COPY WDD311 -PRE BENA-   -RED IO-AREA3.                 
044000     EJECT                                                                
044100 01  DLI-IO-AREA4.                                                        
044200   03  IO-AREA4                 PIC X(928) VALUE SPACE.                   
044300     SKIP3                                                                
044400*  03  WLARTC01  -COPY WDK601              -RED IO-AREA4.                 
044500     EJECT                                                                
044600*  03  WLARTC11  -COPY WDK611              -RED IO-AREA4.                 
044700     EJECT                                                                
044800*  03  WLARTC25  -COPY WDK625              -RED IO-AREA4.                 
044900     EJECT                                                                
045000 01  DLI-IO-AREA5.                                                        
045100   03  IO-AREA5                 PIC X(550)  VALUE SPACE.                  
045200     SKIP3                                                                
045300*  03  WLARTG01  -COPY WDD201 -PRE COPY-   -RED IO-AREA5.                 
045400     EJECT                                                                
045500*  03  WLARTG11  -COPY WDD211 -PRE COPY11- -RED IO-AREA5.                 
045600     EJECT                                                                
045700 LINKAGE SECTION.                                                         
045800*01  -COPY W0009     -PRE MSG-                                            
045900     SKIP2                                                                
046000     EJECT                                                                
046100*01  -COPY W0008     -PRE USEA-                                           
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008     -PRE XXAP-                                           
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008     -PRE ARTG-                                           
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000*01  -COPY W0008     -PRE ARTG-2-                                         
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300*01  -COPY W0008     -PRE ARTJ-                                           
047400     05  FILLER                  PIC X.                                   
047500     EJECT                                                                
047600*01  -COPY W0008     -PRE ARTC-                                           
047700     05  FILLER                  PIC X.                                   
047800     EJECT                                                                
047900*01  -COPY W0008     -PRE ERSB-                                           
048000     05  FILLER                  PIC X.                                   
048100     EJECT                                                                
048200*01  -COPY W0008     -PRE BENA-                                           
048300     05  FILLER                  PIC X.                                   
048400     EJECT                                                                
048500**********************************************                            
048600*                                                                         
048700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
048800                                  XXAP-PCB ARTG-PCB ARTG-2-PCB            
048900                                  ARTJ-PCB ARTC-PCB                       
049000                                  ERSB-PCB BENA-PCB.                      
049100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
049200                                   XXAP-PCB ARTG-PCB ARTG-2-PCB           
049300                                   ARTJ-PCB ARTC-PCB                      
049400                                   ERSB-PCB BENA-PCB.                     
049500**********************************************                            
049600     PERFORM IMS-GET-MSG                                                  
049700     IF SEGMENT-FINNS                                                     
049800        PERFORM A-INIT-SPARA-INPUT                                        
049900        IF WS-IDARTNR NUMERIC                                             
050000           MOVE WS-IDARTNR TO W-IDARTNR                                   
050100                              W-IDARTNR-MIN7                              
050200                              W-IDARTNR-MIN                               
050300                              W-IDARTNR-MAX                               
050400           PERFORM S6-LAES-ARTIKEL-INFO                                   
050500           IF SEGMENT-FINNS                                               
050600              IF MFS-UPDATE                                               
050700                 IF WS-TIBASL = ZERO                                      
050800                    MOVE 'WS-TIBASL > ZERO ' TO MOD-TEMFSINF              
050900                    MOVE FEL-5 (SPRAK-IX) TO MOD-TEMFSFEL                 
051000                 ELSE                                                     
051100                    PERFORM B-KOLLA-INDATA                                
051200                    IF INDATA-OK                                          
051300                       MOVE JA TO UPPDATERING-SW                          
051400                       PERFORM C-UPPDATERA                                
051500                       IF UPPDATERING-OK                                  
051600                          PERFORM D-LAES-SAMMA-MARKNAD                    
051700                          PERFORM S3-RENSA-FAELT-MOD                      
051800                       ELSE                                               
051900                          PERFORM S1-ROER-EJ-FAELT-VISA                   
052000                          PERFORM S2-ROER-EJ-FAELT-IN                     
052100                       END-IF                                             
052200                    ELSE                                                  
052300                       MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL              
052400                       MOVE NEJ TO UPPDATERING-SW                         
052500                       PERFORM S1-ROER-EJ-FAELT-VISA                      
052600                       PERFORM S2-ROER-EJ-FAELT-IN                        
052700                    END-IF                                                
052800                 END-IF                                                   
052900              ELSE                                                        
053000                 IF WS-KDPRODSL = ZERO                                    
053100                    MOVE FEL-6 (SPRAK-IX) TO MOD-TEMFSFEL                 
053200                    MOVE NEJ TO UPPDATERING-SW                            
053300                    PERFORM S1-ROER-EJ-FAELT-VISA                         
053400                    PERFORM S2-ROER-EJ-FAELT-IN                           
053500                 ELSE                                                     
053600                    MOVE JA TO UPPDATERING-SW                             
053700                    IF MFS-IDPFK = '8'                                    
053800                       PERFORM E-KOLLA-PF8                                
053900                    ELSE                                                  
054000                       IF MFS-IDPFK = '7'                                 
054100                          PERFORM F-LAES-FOERSTA-MARKNAD                  
054200                       ELSE                                               
054300                          PERFORM D-LAES-SAMMA-MARKNAD                    
054400                       END-IF                                             
054500                    END-IF                                                
054600                    PERFORM S3-RENSA-FAELT-MOD                            
054700                 END-IF                                                   
054800              END-IF                                                      
054900              IF UPPDATERING-OK                                           
055000                 PERFORM S7-LAES-VISA-INFO-MARKNADER                      
055100                 IF MFS-UPDATE                                            
055200                    MOVE +02 TO MOD-CURSOR-RAD                            
055300                    MOVE +12 TO MOD-CURSOR-KOL                            
055400                 ELSE                                                     
055500                    MOVE +09 TO MOD-CURSOR-RAD                            
055600                    MOVE +10 TO MOD-CURSOR-KOL                            
055700                 END-IF                                                   
055800              END-IF                                                      
055900           ELSE                                                           
056000              PERFORM S3-RENSA-FAELT-MOD                                  
056100              MOVE FEL-10 (SPRAK-IX) TO MOD-TEMFSFEL                      
056200           END-IF                                                         
056300        ELSE                                                              
056400           MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                          
056500           PERFORM S3-RENSA-FAELT-MOD                                     
056600        END-IF                                                            
056700        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
056800        PERFORM IMS-INSERT-MSG                                            
056900     END-IF                                                               
057000*                                                                         
057100     MOVE ZERO TO RETURN-CODE                                             
057200     GOBACK                                                               
057300                                                                          
057400     .                                                                    
057500     EJECT                                                                
057600**********************************************                            
057700*                                                                         
057800 A-INIT-SPARA-INPUT SECTION.                                              
057900*                                                                         
058000**********************************************                            
058100     IF MSG-DUBBLA-TRANSKODER                                             
058200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11401                 
058300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
058400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
058500       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
058600       MOVE MSG-IDPFK TO MFS-IDPFK                                        
058700     ELSE                                                                 
058800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I11401                  
058900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
059000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
059100       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
059200     END-IF                                                               
059300                                                                          
059400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
059500     MOVE '001'             TO MSGI-KDCALL                                
059600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
059700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
059800     MOVE '1114'            TO MSGI-IDTRANS                               
059900                                                                          
060000     IF MFS-IDTRANS NOT = '1114'                                          
060100        MOVE SPACE TO MFS-KDTRTYP                                         
060200        MOVE '7' TO MFS-IDPFK                                             
060300        IF MFS-IDTRANS = '1161'                                           
060400          MOVE MID-W1I11401         TO 1161-MID-W1I16101                  
060500          MOVE 1161-MID-IDPROJ-UT TO MID-IDPROJ-KEY                       
060600          MOVE 1161-MID-IDAO-UT     TO MID-IDAO-KEY                       
060700                                                                          
060800          MOVE +1 TO RAD-IX                                               
060900          PERFORM UNTIL RAD-IX > 42                                       
061000             IF 1161-MID-AFFECT(RAD-IX) NOT = ALL '+'                     
061100                MOVE 1161-MID-IDARTNR(RAD-IX) TO                          
061200                                               MSGI-IDARTNR               
061300                ADD +43                     TO RAD-IX                     
061400             ELSE                                                         
061500                ADD +1                      TO RAD-IX                     
061600             END-IF                                                       
061700          END-PERFORM                                                     
061800        ELSE                                                              
061900          IF MFS-IDTRANS = '9122'                                         
062000            MOVE MID-W1I11401        TO 9122-MID-W9I12201                 
062100            MOVE 9122-MID-IDARTNR-UT TO MSGI-IDARTNR                      
062200          ELSE                                                            
062300            IF (MID-IDARTNR-IN NUMERIC                                    
062400            AND MID-IDARTNR-IN > ZERO)                                    
062500                MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                       
062600            END-IF                                                        
062700          END-IF                                                          
062800        END-IF                                                            
062900     ELSE                                                                 
063000        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
063100     END-IF                                                               
063200                                                                          
063300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063400     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
063500     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
063600                                                                          
063700     IF MID-IDARTNR-IN = ALL '+'                                          
063800        CONTINUE                                                          
063900     ELSE                                                                 
064000        MOVE ' '            TO MFS-KDTRTYP                                
064100        MOVE '7'            TO MFS-IDPFK                                  
064200        IF MFS-IDTRANS = '1114' OR '9122'                                 
064300           MOVE SPACE TO MID-IDPROJ-KEY                                   
064400                         MID-IDAO-KEY                                     
064500        END-IF                                                            
064600     END-IF                                                               
064700                                                                          
064800     IF MSGI-IDLAND-SPR = 'GB'                                            
064900       MOVE +2 TO SPRAK-IX                                                
065000       MOVE 'GB ' TO W-IDSKYLT                                            
065100     ELSE                                                                 
065200       MOVE +1 TO SPRAK-IX                                                
065300       MOVE 'S  ' TO W-IDSKYLT                                            
065400     END-IF                                                               
065500                                                                          
065600                                                                          
065700     MOVE LOW-VALUE TO MSG-AREA                                           
065800     MOVE 'W1O114N1' TO MFS-IDMOD                                         
065900     MOVE '1114' TO MOD-IDTRANS                                           
066000     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
066100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
066200     MOVE MID-IDPROJ-KEY TO MOD-IDPROJ-KEY                                
066300     MOVE MID-IDAO-KEY     TO MOD-IDAO-KEY                                
066400     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
066500     PERFORM MFS-RENSA-MOD-IN                                             
066600                                                                          
066700     ACCEPT WS-DATUM1 FROM DATE                                           
066800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM2                        
066900     PERFORM S3-RENSA-FAELT-MOD                                           
067000                                                                          
067100     .                                                                    
067200     EJECT                                                                
067300**********************************************                            
067400*                                                                         
067500 B-KOLLA-INDATA SECTION.                                                  
067600*                                                                         
067700**********************************************                            
067800     MOVE JA TO INDATA-SW                                                 
067900                                                                          
068000***  MID-FLBASL = EJ UPP PÅ MARKNADSKÖ                                    
068100***  OM DEN SÄTTS TILL J ======> TIBASLM = 1                              
068200     IF MID-FLBASL NOT = ALL '+'                                          
068300        IF MID-FLBASL = 'J' OR SPACE                                      
068400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBASL-IN-ATTR                
068500        ELSE                                                              
068600           MOVE NEJ                TO INDATA-SW                           
068700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBASL-IN-ATTR                  
068800        END-IF                                                            
068900     END-IF                                                               
069000                                                                          
069100*****   MID-FLATERPAKOE = FLAGGA SOM MARKERAR ATT ARTIKELN SKALL          
069200*****   UPP PÅ SAMTLIGA MARKNADERS KÖ-BILD IGEN ALLA AV MARKN UPPD        
069300*****   VÄRDEN SKALL LIGGA KVAR.                                          
069400*****   TISTOMREG SOM SÄTTS TILL DAG-DAT + 4 VECKOR.                      
069500*****   TIBASLM SÄTTS TILL ZERO PÅ SAMTLIGA MARKNADER.                    
069600                                                                          
069700     IF MID-FLATERPAKOE NOT = ALL '+'                                     
069800        IF MID-FLATERPAKOE = 'J'                                          
069900           IF ARTG-ART-FLBASL = 'J'                                       
070000              MOVE NEJ                TO INDATA-SW                        
070100              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLATERPAKOE-IN-ATTR          
070200           ELSE                                                           
070300              MOVE MFS-ALFA-FAELT-RAETT TO                                
070400                                     MOD-FLATERPAKOE-IN-ATTR              
070500           END-IF                                                         
070600        ELSE                                                              
070700           MOVE NEJ                TO INDATA-SW                           
070800           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLATERPAKOE-IN-ATTR             
070900        END-IF                                                            
071000     END-IF                                                               
071100                                                                          
071200                                                                          
071300     IF MID-IDARTNR-MOTSV NOT = ALL '+'                                   
071400        IF MID-IDARTNR-MOTSV NUMERIC                                      
071500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-MOTSV-IN-ATTR          
071600        ELSE                                                              
071700           MOVE NEJ                 TO INDATA-SW                          
071800           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-MOTSV-IN-ATTR          
071900        END-IF                                                            
072000     END-IF                                                               
072100                                                                          
072200     IF MID-IDARTNR-NEW NOT = ALL '+'                                     
072300        IF MID-IDARTNR-NEW NUMERIC                                        
072400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-NEW-ATTR               
072500        ELSE                                                              
072600           MOVE NEJ                 TO INDATA-SW                          
072700           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-NEW-ATTR               
072800        END-IF                                                            
072900     END-IF                                                               
073000                                                                          
073100     IF MID-TEARTNOT-BASL NOT = ALL '+'                                   
073200        IF MID-TEARTNOT-BASL NUMERIC                                      
073300           MOVE NEJ                TO INDATA-SW                           
073400           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-BASL-ATTR              
073500        ELSE                                                              
073600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-BASL-ATTR            
073700        END-IF                                                            
073800     END-IF                                                               
073900                                                                          
074000     IF MID-TEARTNOT-VAR NOT = ALL '+'                                    
074100        IF MID-TEARTNOT-VAR NUMERIC                                       
074200           MOVE NEJ                TO INDATA-SW                           
074300           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-VAR-ATTR               
074400        ELSE                                                              
074500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-VAR-ATTR             
074600        END-IF                                                            
074700     END-IF                                                               
074800                                                                          
074900***********************       HÄR BÖRJAR RELATIONS KONTROLLERNA.          
075000                                                                          
075100     IF MID-IDARTNR-NEW NOT = ALL '+'                                     
075200        IF MID-FLBASL        = ALL '+' AND                                
075300           MID-FLATERPAKOE   = ALL '+' AND                                
075400           MID-TEARTNOT-VAR  = ALL '+' AND                                
075500           MID-TEARTNOT-BASL = ALL '+' AND                                
075600           MID-IDARTNR-MOTSV = ALL '+'                                    
075700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-NEW-ATTR               
075800        ELSE                                                              
075900           MOVE NEJ TO INDATA-SW                                          
076000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NEW-ATTR                 
076100        END-IF                                                            
076200     END-IF                                                               
076300                                                                          
076400     IF MID-FLATERPAKOE NOT = ALL '+'                                     
076500        IF MID-FLBASL        = ALL '+' AND                                
076600           MID-IDARTNR-NEW   = ALL '+'                                    
076700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLATERPAKOE-IN-ATTR           
076800        ELSE                                                              
076900           MOVE NEJ TO INDATA-SW                                          
077000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLATERPAKOE-IN-ATTR             
077100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBASL-IN-ATTR                  
077200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-NEW-ATTR                 
077300        END-IF                                                            
077400     END-IF                                                               
077500                                                                          
077600****HÄR BÖRJAR KONTROLLEN PÅ RADERNA.                                     
077700                                                                          
077800     MOVE +1    TO RAD-IX                                                 
077900     MOVE +1    TO COL-IX                                                 
078000     MOVE NEJ   TO RADUPPDAT-SW                                           
078100     MOVE NEJ   TO TIDUPPDAT-SW                                           
078200                                                                          
078300     PERFORM UNTIL RAD-IX > 9                                             
078400        PERFORM UNTIL COL-IX > 4                                          
078500                                                                          
078600           IF MID-AFFECT(RAD-IX COL-IX) NOT = ALL '+'                     
078700              IF MID-AFFECT(RAD-IX COL-IX) = 'J' OR 'N'                   
078800                 MOVE JA    TO RADUPPDAT-SW                               
078900                 MOVE MFS-ALFA-FAELT-RAETT TO                             
079000                             MOD-AFFECT-ATTR(RAD-IX COL-IX)               
079100                 IF MID-AFFECT(RAD-IX COL-IX) = 'J'                       
079200                    MOVE JA    TO TIDUPPDAT-SW                            
079300                 END-IF                                                   
079400              ELSE                                                        
079500                 MOVE NEJ                TO INDATA-SW                     
079600                 MOVE MFS-ALFA-FAELT-FEL TO                               
079700                               MOD-AFFECT-ATTR(RAD-IX COL-IX)             
079800              END-IF                                                      
079900           END-IF                                                         
080000           ADD +1            TO COL-IX                                    
080100        END-PERFORM                                                       
080200        ADD +1               TO RAD-IX                                    
080300        MOVE +1              TO COL-IX                                    
080400     END-PERFORM                                                          
080500     .                                                                    
080600     EJECT                                                                
080700**********************************************                            
080800*                                                                         
080900 C-UPPDATERA       SECTION.                                               
081000                                                                          
081100*****************************************************                     
081200* HÄR SKER EN KONTROLL OM DET ÄR KOPIERING                                
081300* ELLER ANNAN TYP AV UPPDATERING.                                         
081400* SKALL DATUM ODYL LÄGGAS IN VID KOPIERING????                            
081500*****************************************************                     
081600     MOVE 002      TO WORK-KDCALL                                         
081700     MOVE 20       TO WORK-KVWORKD                                        
081800     MOVE WC-CDC-SE TO WORK-IDDC                                          
081900     MOVE WS-DATUM1 TO WORK-TIAAMMDD-FOM                                  
082000     CALL WORKDAY  USING WORK-KDCALL                                      
082100                         WORK-DATE-AREA                                   
082200                         WORK-KDSVAR                                      
082300                                                                          
082400     IF MID-IDARTNR-NEW = ALL '+'                                         
082500        PERFORM CA-UPPD-ARTIKEL                                           
082600     ELSE                                                                 
082700        PERFORM CB-KOPIERA-ARTIKEL                                        
082800        IF UPPDATERING-OK                                                 
082900           PERFORM CC-VISA-KOP-ART-VAERDEN                                
083000        END-IF                                                            
083100        MOVE MID-IDARTNR-NEW TO W-IDARTNR                                 
083200        MOVE ALL '+' TO MSGI-WMSGINIT                                     
083300        MOVE '001'             TO MSGI-KDCALL                             
083400        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
083500        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
083600        MOVE '1114'            TO MSGI-IDTRANS                            
083700        MOVE W-IDARTNR         TO MSGI-IDARTNR                            
083800        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200*****************************************************                     
084300*                                                                         
084400 CA-UPPD-ARTIKEL       SECTION.                                           
084500*                                                                         
084600*****************************************************                     
084700                                                                          
084800     IF MID-TEARTNOT-VAR NOT = ALL '+'                                    
084900        PERFORM IMS-GU-ARTC01-4                                           
085000        MOVE 6           TO W-KDNOTTYP                                    
085100        PERFORM IMS-GHNP-NOTTYP-ARTC25-UNIK-4                             
085200        IF SEGMENT-FINNS                                                  
085300           MOVE MID-TEARTNOT-VAR TO NOT-TEARTNOT                          
085400                                    MOD-TEARTNOT-VAR                      
085500           PERFORM IMS-REPL-ARTC25-4                                      
085600        ELSE                                                              
085700           MOVE 6  TO WORK-NOT-KDNOTTYP                                   
085800           MOVE MID-TEARTNOT-VAR TO WORK-NOT-TEARTNOT                     
085900                                    MOD-TEARTNOT-VAR                      
086000           MOVE WORK-NOT-WDK625 TO NOT-WDK625                             
086100           PERFORM IMS-ISRT-ARTC25-4                                      
086200        END-IF                                                            
086300     END-IF                                                               
086400                                                                          
086500     EVALUATE TRUE ALSO TRUE ALSO TRUE                                    
086600     WHEN MID-TEARTNOT-BASL = ALL '+' ALSO                                
086700          MID-IDARTNR-MOTSV = ALL '+' ALSO                                
086800          MID-FLBASL = ALL '+'                                            
086900          CONTINUE                                                        
087000     WHEN OTHER                                                           
087100        PERFORM IMS-GHU-ARTG01-3                                          
087200        IF MID-TEARTNOT-BASL NOT = ALL '+'                                
087300           MOVE MID-TEARTNOT-BASL TO ARTG-ART-TEARTNOT-BASL               
087400                                     MOD-TEARTNOT-BASL                    
087500        END-IF                                                            
087600        IF MID-IDARTNR-MOTSV NOT = ALL '+'                                
087700           MOVE MID-IDARTNR-MOTSV TO ARTG-ART-IDARTNR-MOTSV               
087800                                     MOD-IDARTNR-MOTSV-UT                 
087900        END-IF                                                            
088000        IF MID-FLBASL NOT = ALL '+'                                       
088100           MOVE MID-FLBASL TO ARTG-ART-FLBASL                             
088200                              MOD-FLBASL                                  
088300        END-IF                                                            
088400        PERFORM IMS-REPL-ARTG01-3                                         
088500        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
088600        IF MID-FLBASL NOT = ALL '+'                                       
088700           IF MID-FLBASL = 'J'                                            
088800              PERFORM CAD-ARTIKEL-NED-FRAN-MARKNKOE                       
088900              PERFORM CAA-UPPDATERA-DATUM                                 
089000           END-IF                                                         
089100        END-IF                                                            
089200     END-EVALUATE                                                         
089300                                                                          
089400     IF MID-FLATERPAKOE NOT = ALL '+'                                     
089500        PERFORM IMS-GHU-ARTG01-3                                          
089600        IF ARTG-ART-FLBASL = SPACE                                        
089700           PERFORM CAA-UPPDATERA-DATUM                                    
089800           PERFORM CAB-ARTIKEL-UPP-PA-MARKNKOE                            
089900        ELSE                                                              
090000           MOVE FEL-5 (SPRAK-IX) TO MOD-TEMFSFEL                          
090100        END-IF                                                            
090200     ELSE                                                                 
090300        IF RADUPPDAT-OK                                                   
090400           PERFORM CAC-UPPDATERA-RADERNA                                  
090500           IF TIDUPPDAT-OK                                                
090600              PERFORM CAA-UPPDATERA-DATUM                                 
090700           END-IF                                                         
090800        END-IF                                                            
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200**********************************************************                
091300*                                                                         
091400 CAA-UPPDATERA-DATUM SECTION.                                             
091500*                                                                         
091600**********************************************************                
091700*    MOVE MED-11(SPRAK-IX) TO MOD-TEMFSFEL                                
091800     PERFORM IMS-GHU-PROJ-XXAP01-1                                        
091900     PERFORM IMS-GHNP-PROJ-XXAP11-OKV-1                                   
092000     MOVE 1124-TISTAMREG  TO WS-GENERELL-TISTAMREG                        
092100     MOVE 1124-TIPROJSTO  TO WS-GENERELL-TIPROJSTO                        
092200                                                                          
092300     IF 1124-TIBLREG = ZERO                                               
092400        MOVE WS-DATUM1 TO 1124-TIBLREG                                    
092500        PERFORM IMS-REPL-PROJ-XXAP11-1                                    
092600     END-IF                                                               
092700*                                                                         
092800     PERFORM IMS-GHU-ARTG01-3                                             
092900     IF ARTG-ART-DABASL = 1                                               
093000        MOVE WS-DATUM2    TO ARTG-ART-DABASL                              
093100     END-IF                                                               
093200     MOVE ARTG-ART-DABASL (3:6) TO MOD-TIBASL                             
093300                                                                          
093400     MOVE WS1-KDPRODSL             TO TEST-KDPRODSL                       
093500     IF KDPRODSL-ACC OR KDPRODSL-WHEELS                                   
093600        MOVE WS-GENERELL-TIPROJSTO TO ARTG-ART-TISTOMREG                  
093700     ELSE                                                                 
093800***    *******************************************************            
093900***    *SKALL ADDERA +4 VECKOR TILL DAGENS DATUM             *            
094000***    *DVS   20 ST ARBETSDAGAR OM KDPRODSL INTE ÄR 15,16    *            
094100***    *DENNA ADDERING ÄR REDAN GJORD I C- SECTION.          *            
094200***    *******************************************************            
094300        IF WORK-KDSVAR-OK                                                 
094400           MOVE WORK-TIAAMMDD-TOM       TO TMP1-YYMMDD                    
094500           MOVE WS-GENERELL-TISTAMREG   TO TMP2-YYMMDD                    
094600           PERFORM WY2000P1                                               
094700           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
094800              MOVE WS-GENERELL-TISTAMREG TO ARTG-ART-TISTOMREG            
094900           ELSE                                                           
095000              MOVE WORK-TIAAMMDD-TOM       TO TMP1-YYMMDD                 
095100              MOVE WS-GENERELL-TIPROJSTO   TO TMP2-YYMMDD                 
095200              PERFORM WY2000P1                                            
095300              IF TMP1-YYMMDD < TMP2-YYMMDD                                
095400                 MOVE WORK-TIAAMMDD-TOM TO ARTG-ART-TISTOMREG             
095500              ELSE                                                        
095600                 MOVE WS-GENERELL-TIPROJSTO TO ARTG-ART-TISTOMREG         
095700              END-IF                                                      
095800           END-IF                                                         
095900        ELSE                                                              
096000           MOVE WS-GENERELL-TIPROJSTO TO ARTG-ART-TISTOMREG               
096100        END-IF                                                            
096200     END-IF                                                               
096300     PERFORM IMS-REPL-ARTG01-3                                            
096400                                                                          
096500* HÄR BÖRJAR MARKNADSSEGMENTENS TISTOMREG-UPPDATERING                     
096600                                                                          
096700     PERFORM IMS-GHU-ARTG01-3                                             
096800     PERFORM IMS-GHNP-ARTG11-FOERSTA-3                                    
096900                                                                          
097000     PERFORM UNTIL NOT SEGMENT-FINNS                                      
097100       MOVE ARTG11-ART-KDBASLM TO W-1126-KDBASLM, W-KDBASLM               
097200       PERFORM IMS-GNP-PROJ-XXAP12-UNIK-2                                 
097300       IF SEGMENT-FINNS                                                   
097400         MOVE 1126-TIPROJSTO TO WS-MARKNAD-TIPROJSTO                      
097500         MOVE 1126-TISTAMREG TO WS-MARKNAD-TISTAMREG                      
097600         MOVE WS1-KDPRODSL           TO TEST-KDPRODSL                     
097700         IF KDPRODSL-ACC OR KDPRODSL-WHEELS                               
097800           MOVE WS-MARKNAD-TIPROJSTO TO ARTG11-ART-TISTOMREG              
097900         ELSE                                                             
098000           IF WORK-KDSVAR-OK                                              
098100             MOVE WORK-TIAAMMDD-TOM      TO TMP1-YYMMDD                   
098200             MOVE WS-MARKNAD-TISTAMREG   TO TMP2-YYMMDD                   
098300             PERFORM WY2000P1                                             
098400             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
098500               MOVE WS-MARKNAD-TISTAMREG TO ARTG11-ART-TISTOMREG          
098600             ELSE                                                         
098700               MOVE WORK-TIAAMMDD-TOM      TO TMP1-YYMMDD                 
098800               MOVE WS-MARKNAD-TIPROJSTO   TO TMP2-YYMMDD                 
098900               PERFORM WY2000P1                                           
099000               IF TMP1-YYMMDD < TMP2-YYMMDD                               
099100                 MOVE WORK-TIAAMMDD-TOM TO ARTG11-ART-TISTOMREG           
099200               ELSE                                                       
099300                MOVE WS-MARKNAD-TIPROJSTO TO ARTG11-ART-TISTOMREG         
099400               END-IF                                                     
099500             END-IF                                                       
099600           ELSE                                                           
099700             MOVE WS-MARKNAD-TIPROJSTO TO ARTG11-ART-TISTOMREG            
099800           END-IF                                                         
099900         END-IF                                                           
100000       ELSE                                                               
100100         MOVE +0 TO ARTG11-ART-TISTOMREG                                  
100200       END-IF                                                             
100300       PERFORM IMS-REPL-ARTG11-3                                          
100400       PERFORM IMS-GHNP-ARTG11-NEXT-GT-3                                  
100500     END-PERFORM                                                          
100600     .                                                                    
100700     EJECT                                                                
100800**********************************************                            
100900*                                                                         
101000 CAB-ARTIKEL-UPP-PA-MARKNKOE SECTION.                                     
101100*                                                                         
101200**********************************************                            
101300* LÄGGER UPP ARTIKELN PÅ MARKNADENS KÖ IGEN                               
101400* GENOM ATT SÄTTA TIBASLM = ZERO                                          
101500* DETTA FLAGGAS GENOM ATT SÄTTA FLBLMQ TILL J                             
101600**********************************************                            
101700     PERFORM IMS-GHU-ARTG01-3                                             
101800     PERFORM IMS-GHNP-ARTG11-FOERSTA-3                                    
101900     PERFORM UNTIL SEGMENT-SAKNAS                                         
102000        IF ARTG11-ART-TIBASLM NOT = ZERO                                  
102100           MOVE MED-9(SPRAK-IX) TO MOD-TEMFSFEL                           
102200           MOVE ZERO            TO ARTG11-ART-TIBASLM                     
102300        ELSE                                                              
102400           IF MID-IDARTNR-NEW = ALL '+'                                   
102500              MOVE JA               TO ARTG11-ART-FLBLMQ                  
102600              MOVE MED-9 (SPRAK-IX) TO MOD-TEMFSFEL                       
102700           ELSE                                                           
102800              MOVE MED-12(SPRAK-IX) TO MOD-TEMFSFEL                       
102900***           ANROPAS FRÅN CB-KOPIERA.                                    
103000           END-IF                                                         
103100        END-IF                                                            
103200        PERFORM IMS-REPL-ARTG11-3                                         
103300        PERFORM IMS-GHNP-ARTG11-OKVAL-3                                   
103400     END-PERFORM                                                          
103500     .                                                                    
103600     EJECT                                                                
103700**********************************************                            
103800*                                                                         
103900 CAC-UPPDATERA-RADERNA  SECTION.                                          
104000*                                                                         
104100**********************************************                            
104200     PERFORM IMS-GHU-ARTG01-3                                             
104300     MOVE ARTG-ART-FLBASL TO WS-FLBASL                                    
104400     MOVE ZERO  TO WS-TA-BORT-ANT                                         
104500     MOVE +1  TO RAD-IX                                                   
104600     MOVE +1  TO COL-IX                                                   
104700     PERFORM UNTIL RAD-IX > 9                                             
104800        PERFORM UNTIL COL-IX > 4                                          
104900           IF MID-AFFECT(RAD-IX COL-IX) NOT = ALL '+'                     
105000              MOVE MID-KDBASLM(RAD-IX COL-IX) TO W-KDBASLM                
105100              PERFORM IMS-GHNP-ARTG11-UNIK-3                              
105200                                                                          
105300              IF SEGMENT-FINNS                                            
105400                 IF MID-AFFECT(RAD-IX COL-IX) = 'N'                       
105500                   COMPUTE WS-TA-BORT-ANT = WS-TA-BORT-ANT +              
105600                                            ARTG11-ART-KVBASLM            
105700                   PERFORM IMS-DLET-ARTG11-3                              
105800                 ELSE                                                     
105900                   MOVE +0 TO ARTG11-ART-TIBASLM                          
106000                   MOVE JA TO ARTG11-ART-FLBLMQ                           
106100******             *  UPP PÅ KÖ FÖR MARKNADENS-KVANTITETSUPPDAT           
106200                   PERFORM IMS-REPL-ARTG11-3                              
106300                 END-IF                                                   
106400              ELSE                                                        
106500                 IF MID-AFFECT(RAD-IX COL-IX) = 'J'                       
106600                    MOVE MID-KDBASLM(RAD-IX COL-IX)                       
106700                                             TO W-1126-KDBASLM            
106800                    PERFORM CACA-LAEGG-UPP-ARTG11                         
106900                    PERFORM IMS-ISRT-ARTG11-3                             
107000                 END-IF                                                   
107100              END-IF                                                      
107200           END-IF                                                         
107300           ADD +1   TO COL-IX                                             
107400        END-PERFORM                                                       
107500        ADD +1   TO RAD-IX                                                
107600        MOVE +1  TO COL-IX                                                
107700     END-PERFORM                                                          
107800     IF WS-TA-BORT-ANT > ZERO                                             
107900        PERFORM IMS-GHU-ARTG01-3                                          
108000        SUBTRACT WS-TA-BORT-ANT FROM ARTG-ART-KVBASL                      
108100        PERFORM IMS-REPL-ARTG01-3                                         
108200     END-IF                                                               
108300     .                                                                    
108400     EJECT                                                                
108500**********************************************                            
108600*                                                                         
108700 CACA-LAEGG-UPP-ARTG11      SECTION.                                      
108800*                                                                         
108900********      LÄS UNIK MARKNAD PÅ PROJEKTBASEN                            
109000     PERFORM IMS-GHU-PROJ-XXAP01-1                                        
109100     PERFORM IMS-GNP-PROJ-XXAP12-UNIK-2                                   
109200     MOVE MID-KDBASLM(RAD-IX COL-IX) TO WORK-ART-KDBASLM                  
109300                                                                          
109400     MOVE +1 TO DIST-INDX                                                 
109500     PERFORM UNTIL DIST-INDX > 6                                          
109600        MOVE 1126-IDDISTR(DIST-INDX) TO                                   
109700                                 WORK-ART-IDDISTR(DIST-INDX)              
109800        MOVE +0 TO WORK-ART-KVBASLMD(DIST-INDX)                           
109900        ADD  +1 TO DIST-INDX                                              
110000     END-PERFORM                                                          
110100                                                                          
110200     MOVE SPACE TO WORK-ART-KDDEALER                                      
110300                   WORK-ART-TEARTNOT-MARK                                 
110400                                                                          
110500     MOVE +0    TO WORK-ART-KVBASLKIT                                     
110600                   WORK-ART-KVBASLM                                       
110700                   WORK-ART-TISTOMREG                                     
110800                                                                          
110900     MOVE NEJ         TO WORK-ART-FLBLMQ                                  
111000     MOVE WS-IDPROJ   TO WORK-ART-IDPROJ                                  
111100     MOVE WS-KDBPSR   TO WORK-ART-KDBPSR                                  
111200     MOVE WS-IDFKNGRP TO WORK-ART-IDFKNGRP                                
111300                                                                          
111400     IF MID-FLBASL = 'J'                                                  
111500        MOVE +1  TO WORK-ART-TIBASLM                                      
111600     ELSE                                                                 
111700        IF WS-FLBASL = 'J'                                                
111800           MOVE +1  TO WORK-ART-TIBASLM                                   
111900        ELSE                                                              
112000           MOVE +0  TO WORK-ART-TIBASLM                                   
112100        END-IF                                                            
112200     END-IF                                                               
112300                                                                          
112400     MOVE WORK-ART-WDD211 TO ARTG11-ART-WDD211                            
112500     .                                                                    
112600     EJECT                                                                
112700**********************************************                            
112800*                                                                         
112900 CAD-ARTIKEL-NED-FRAN-MARKNKOE SECTION.                                   
113000*                                                                         
113100**********************************************                            
113200     PERFORM IMS-GHU-ARTG01-3                                             
113300     PERFORM IMS-GHNP-ARTG11-FOERSTA-3                                    
113400     PERFORM UNTIL SEGMENT-SAKNAS                                         
113500        IF ARTG11-ART-TIBASLM = ZERO                                      
113600           MOVE MED-10 (SPRAK-IX) TO MOD-TEMFSFEL                         
113700           MOVE +1 TO ARTG11-ART-TIBASLM                                  
113800           MOVE JA TO ARTG11-ART-FLBLMQ                                   
113900           PERFORM IMS-REPL-ARTG11-3                                      
114000        END-IF                                                            
114100        PERFORM IMS-GHNP-ARTG11-OKVAL-3                                   
114200     END-PERFORM                                                          
114300     .                                                                    
114400     EJECT                                                                
114500**********************************************                            
114600*                                                                         
114700 CB-KOPIERA-ARTIKEL       SECTION.                                        
114800*                                                                         
114900******************************************************                    
115000* ALLA MARKNADER KOPIERAS ÖVER FRÅN DEN GAMLA        *                    
115100* TILL DEN NYA ARTIKEL.                              *                    
115200* LIKASÅ TEARTNOT-BASL.                              *                    
115300* OM DET INTE FINNS MARKNADER PÅ DEN NYA ARTIKELN !! *                    
115400******************************************************                    
115500     MOVE SPACE              TO WS-FLBASL                                 
115600     MOVE MID-IDARTNR-NEW    TO W-IDARTNR                                 
115700     PERFORM IMS-GHU-ARTG01-3                                             
115800     IF SEGMENT-FINNS                                                     
115900        IF ARTG-ART-DABASL = ZERO                                         
116000           MOVE FEL-5 (SPRAK-IX)  TO MOD-TEMFSFEL                         
116100           MOVE NEJ               TO UPPDATERING-SW                       
116200        ELSE                                                              
116300           MOVE ARTG-ART-IDPROJ   TO WS-IDPROJ                            
116400                                     MOD-IDPROJ                           
116500                                     W-IDPROJ                             
116600                                     W-1123-IDPROJ                        
116700           MOVE ARTG-ART-IDAO     TO WS-IDAO                              
116800           IF MID-IDAO-KEY = SPACE AND MID-IDPROJ-KEY = SPACE             
116900              MOVE ARTG-ART-IDAO     TO MOD-IDAO-KEY                      
117000              MOVE ARTG-ART-IDPROJ   TO MOD-IDPROJ-KEY                    
117100           END-IF                                                         
117200           MOVE MID-IDAO-KEY       TO W-IDAO-MIN                          
117300                                      W-IDAO-MAX                          
117400*                                                                         
117500           IF ARTG-ART-IDARTNR-MOTSV = ZERO                               
117600              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-MOTSV-UT                
117700           ELSE                                                           
117800              MOVE ARTG-ART-IDARTNR-MOTSV TO                              
117900                                      MOD-IDARTNR-MOTSV-UT                
118000              INSPECT MOD-IDARTNR-MOTSV-UT REPLACING LEADING              
118100                             ZERO BY SPACE                                
118200           END-IF                                                         
118300** HÄMTAR BPSR IDFKNGRP FRÅN ARTC                                         
118400           PERFORM IMS-GU-ARTC01-4                                        
118500           PERFORM S8-ARTC-IDFKNGRP-PRODSL                                
118600           PERFORM IMS-GHU-PROJ-XXAP01-1                                  
118700           IF SEGMENT-FINNS                                               
118800              PERFORM IMS-GHNP-PROJ-XXAP11-OKV-1                          
118900              IF SEGMENT-FINNS                                            
119000                 MOVE WS-IDARTNR        TO WX-IDARTNR                     
119100                 PERFORM IMS-GHU-ARTG01-5                                 
119200                 IF SEGMENT-FINNS                                         
119300                    MOVE COPY-ART-FLBASL TO WS-FLBASL                     
119400                                            ARTG-ART-FLBASL               
119500                                            MOD-FLBASL                    
119600                    MOVE COPY-ART-TEARTNOT-BASL  TO                       
119700                                         ARTG-ART-TEARTNOT-BASL           
119800                                         MOD-TEARTNOT-BASL                
119900                    PERFORM IMS-REPL-ARTG01-3                             
120000                    PERFORM IMS-GHNP-ARTG11-FOERSTA-3                     
120100                    IF SEGMENT-SAKNAS                                     
120200                       PERFORM IMS-GHU-ARTG01-3                           
120300                       PERFORM IMS-GHNP-ARTG11-FOERSTA-5                  
120400                       PERFORM UNTIL SEGMENT-SAKNAS                       
120500                          PERFORM CB1-KOPIERA-EN-MARKNAD                  
120600                          PERFORM IMS-GHNP-ARTG11-OKVAL-5                 
120700                       END-PERFORM                                        
120800                    ELSE                                                  
120900****    ********************************************************          
121000****    * KOPIERING DÄR MARKNADER SOM FINNS PÅ COPY.ART SKALL  *          
121100****    * LÄGGAS ÖVER TILL DEN NYA ARTIKELN. KOLLA FLBASL !!   *          
121200****    *  NYA ARTIKELNS ROT OCH FÖRSTA BARN ÄR LÄST. (AREA-3) *          
121300****    ********************************************************          
121400                       PERFORM IMS-GHNP-ARTG11-FOERSTA-5                  
121500                       IF SEGMENT-FINNS                                   
121600                         MOVE ZERO   TO WS-TA-BORT-ANT                    
121700                         MOVE JA     TO FORTS-COPY                        
121800                         PERFORM UNTIL FORTS-COPY = NEJ                   
121900                           PERFORM CBA-MATCHA-MARKNAD-KOPIERA             
122000                         END-PERFORM                                      
122100                         IF WS-TA-BORT-ANT > ZERO                         
122200                           PERFORM IMS-GHU-ARTG01-3                       
122300                           SUBTRACT WS-TA-BORT-ANT FROM                   
122400                                             ARTG-ART-KVBASL              
122500                           PERFORM IMS-REPL-ARTG01-3                      
122600                         END-IF                                           
122700                       ELSE                                               
122800                         MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL            
122900                         MOVE NEJ  TO UPPDATERING-SW                      
123000                       END-IF                                             
123100                    END-IF                                                
123200                    MOVE WS-IDARTNR TO W-IDARTNR                          
123300                    PERFORM IMS-GU-ARTC01-4                               
123400                    MOVE +6 TO W-KDNOTTYP                                 
123500                    PERFORM IMS-GHNP-NOTTYP-ARTC25-UNIK-4                 
123600                    IF SEGMENT-FINNS                                      
123700                       MOVE NOT-TEARTNOT TO                               
123800                                         WS-TEARTNOT-VARNOT               
123900                       MOVE MID-IDARTNR-NEW TO W-IDARTNR                  
124000                       PERFORM IMS-GU-ARTC01-4                            
124100                       MOVE 6                  TO W-KDNOTTYP              
124200                       PERFORM IMS-GHNP-NOTTYP-ARTC25-UNIK-4              
124300                       IF SEGMENT-FINNS                                   
124400                          MOVE WS-TEARTNOT-VARNOT TO                      
124500                                              NOT-TEARTNOT                
124600                                              MOD-TEARTNOT-VAR            
124700                          PERFORM IMS-REPL-ARTC25-4                       
124800                       ELSE                                               
124900                          MOVE 6             TO WORK-NOT-KDNOTTYP         
125000                          MOVE WS-TEARTNOT-VARNOT TO                      
125100                                               WORK-NOT-TEARTNOT          
125200                                               MOD-TEARTNOT-VAR           
125300                          MOVE WORK-NOT-WDK625 TO NOT-WDK625              
125400                          PERFORM IMS-ISRT-ARTC25-4                       
125500                       END-IF                                             
125600                    END-IF                                                
125700                    PERFORM CAA-UPPDATERA-DATUM                           
125800                    PERFORM CAB-ARTIKEL-UPP-PA-MARKNKOE                   
125900                 ELSE                                                     
126000                    MOVE FEL-10 (SPRAK-IX)  TO MOD-TEMFSFEL               
126100                    MOVE NEJ                TO UPPDATERING-SW             
126200                 END-IF                                                   
126300              ELSE                                                        
126400                 MOVE FEL-8  (SPRAK-IX)  TO MOD-TEMFSFEL                  
126500                 MOVE NEJ                TO UPPDATERING-SW                
126600              END-IF                                                      
126700           ELSE                                                           
126800              MOVE FEL-8  (SPRAK-IX)  TO MOD-TEMFSFEL                     
126900              MOVE NEJ                TO UPPDATERING-SW                   
127000           END-IF                                                         
127100        END-IF                                                            
127200     ELSE                                                                 
127300        MOVE FEL-10 (SPRAK-IX)  TO MOD-TEMFSFEL                           
127400        MOVE NEJ                TO UPPDATERING-SW                         
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800**********************************************                            
127900*                                                                         
128000 CBA-MATCHA-MARKNAD-KOPIERA SECTION.                                      
128100*                                                                         
128200**********************************************                            
128300     IF COPY11-ART-KDBASLM = HIGH-VALUE AND                               
128400        ARTG11-ART-KDBASLM = HIGH-VALUE                                   
128500        MOVE NEJ     TO FORTS-COPY                                        
128600     ELSE                                                                 
128700        IF COPY11-ART-KDBASLM < ARTG11-ART-KDBASLM                        
128800           PERFORM CB1-KOPIERA-EN-MARKNAD                                 
128900           MOVE COPY11-ART-KDBASLM TO W-KDBASLM                           
129000           PERFORM IMS-GHU-ARTG01-3                                       
129100           PERFORM IMS-GHNP-ARTG11-UNIK-NXT-3                             
129200           PERFORM IMS-GHNP-ARTG11-OKVAL-3                                
129300           IF SEGMENT-SAKNAS                                              
129400              MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                       
129500           END-IF                                                         
129600           PERFORM IMS-GHNP-ARTG11-OKVAL-5                                
129700           IF SEGMENT-SAKNAS                                              
129800              MOVE HIGH-VALUE TO COPY11-ART-KDBASLM                       
129900           END-IF                                                         
130000        ELSE                                                              
130100           IF COPY11-ART-KDBASLM >  ARTG11-ART-KDBASLM                    
130200              COMPUTE WS-TA-BORT-ANT = WS-TA-BORT-ANT +                   
130300                                       ARTG11-ART-KVBASLM                 
130400              PERFORM IMS-DLET-ARTG11-3                                   
130500              PERFORM IMS-GHNP-ARTG11-OKVAL-3                             
130600              IF SEGMENT-SAKNAS                                           
130700                 MOVE HIGH-VALUE TO  ARTG11-ART-KDBASLM                   
130800              END-IF                                                      
130900           ELSE                                                           
131000              IF COPY11-ART-KDBASLM = ARTG11-ART-KDBASLM                  
131100                 IF WS-FLBASL = 'J'                                       
131200                    IF ARTG11-ART-TIBASLM = +1                            
131300                       CONTINUE                                           
131400                    ELSE                                                  
131500                       MOVE +1 TO                                         
131600                       ARTG11-ART-TIBASLM                                 
131700                       MOVE NEJ TO ARTG11-ART-FLBLMQ                      
131800                       PERFORM IMS-REPL-ARTG11-3                          
131900                    END-IF                                                
132000                 ELSE                                                     
132100                    IF ARTG11-ART-TIBASLM > ZERO                          
132200                       MOVE NEJ  TO  ARTG11-ART-FLBLMQ                    
132300                       MOVE ZERO TO  ARTG11-ART-TIBASLM                   
132400                       PERFORM IMS-REPL-ARTG11-3                          
132500                    END-IF                                                
132600                 END-IF                                                   
132700                 PERFORM IMS-GHNP-ARTG11-OKVAL-5                          
132800                 IF SEGMENT-SAKNAS                                        
132900                    MOVE HIGH-VALUE TO  COPY11-ART-KDBASLM                
133000                 END-IF                                                   
133100                 PERFORM IMS-GHNP-ARTG11-OKVAL-3                          
133200                 IF SEGMENT-SAKNAS                                        
133300                    MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                 
133400                 END-IF                                                   
133500              END-IF                                                      
133600           END-IF                                                         
133700        END-IF                                                            
133800     END-IF                                                               
133900     .                                                                    
134000     EJECT                                                                
134100**********************************************                            
134200*                                                                         
134300 CB1-KOPIERA-EN-MARKNAD SECTION.                                          
134400*                                                                         
134500**********************************************                            
134600                                                                          
134700     MOVE COPY11-ART-KDBASLM   TO WS1-ART-KDBASLM                         
134800     MOVE COPY11-ART-TISTOMREG TO WS1-ART-TISTOMREG                       
134900                                                                          
135000     MOVE SPACE                TO WS1-ART-KDDEALER                        
135100                                  WS1-ART-TEARTNOT-MARK                   
135200     MOVE ZERO                 TO WS1-ART-KVBASLKIT                       
135300                                  WS1-ART-KVBASLM                         
135400     IF WS-FLBASL = 'J'                                                   
135500        MOVE +1       TO WS1-ART-TIBASLM                                  
135600     ELSE                                                                 
135700        MOVE ZERO     TO WS1-ART-TIBASLM                                  
135800     END-IF                                                               
135900                                                                          
136000     MOVE NEJ         TO WS1-ART-FLBLMQ                                   
136100     MOVE WS-IDPROJ   TO WS1-ART-IDPROJ                                   
136200     MOVE WS-KDBPSR   TO WS1-ART-KDBPSR                                   
136300     MOVE WS-IDFKNGRP TO WS1-ART-IDFKNGRP                                 
136400                                                                          
136500     MOVE +1 TO KOP-INDX                                                  
136600     PERFORM UNTIL KOP-INDX > 6                                           
136700        MOVE COPY11-ART-IDDISTR(KOP-INDX) TO                              
136800                                     WS1-ART-IDDISTR(KOP-INDX)            
136900        MOVE ZERO TO WS1-ART-KVBASLMD(KOP-INDX)                           
137000        ADD +1 TO KOP-INDX                                                
137100     END-PERFORM                                                          
137200     MOVE WS1-ART-WDD211 TO ARTG11-ART-WDD211                             
137300     PERFORM IMS-ISRT-ARTG11-3                                            
137400     .                                                                    
137500     EJECT                                                                
137600**********************************************                            
137700*                                                                         
137800 CC-VISA-KOP-ART-VAERDEN SECTION.                                         
137900*                                                                         
138000**********************************************                            
138100* ENTER TRYCKNING                                                         
138200**********************************************                            
138300     MOVE MID-IDARTNR-NEW TO MOD-IDARTNR-UT                               
138400                             W-IDARTNR-MIN7                               
138500                             W-IDARTNR-MAX                                
138600                             W-IDARTNR                                    
138700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
138800     PERFORM IMS-GU-ARTC01-4                                              
138900     IF ART-FLERS = JA                                                    
139000        PERFORM IMS-GU-ERSB01-MINMAX-3                                    
139100        IF SEGMENT-FINNS                                                  
139200           IF ERSB01-ERS-IDARTNR NOT = ZERO                               
139300              MOVE ERSB01-ERS-IDARTNR TO W-IDARTNR                        
139400              PERFORM IMS-GU-ARTC01-4                                     
139500              PERFORM IMS-GNP-ARTC11-4                                    
139600              IF CLAG-FLLSRDEL = 'N'                                      
139700                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                  
139800              ELSE                                                        
139900                 MOVE ERSB01-ERS-IDARTNR TO MOD-IDARTNR-ERS               
140000                 INSPECT MOD-IDARTNR-ERS REPLACING LEADING                
140100                                         ZERO BY SPACE                    
140200              END-IF                                                      
140300           ELSE                                                           
140400              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                     
140500           END-IF                                                         
140600        ELSE                                                              
140700           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                        
140800        END-IF                                                            
140900     ELSE                                                                 
141000        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                           
141100     END-IF                                                               
141200     PERFORM IMS-GU-ARTC01-4                                              
141300     IF ART-KDERS-UTG = ZERO                                              
141400        PERFORM IMS-GNP-ARTC11-4                                          
141500        MOVE CLAG-KDERS  TO MOD-KDERS                                     
141600     ELSE                                                                 
141700        MOVE ART-KDERS-UTG TO MOD-KDERS                                   
141800     END-IF                                                               
141900     PERFORM IMS-GU-BENA11-BSEQ-3                                         
142000     MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                                
142100     .                                                                    
142200     EJECT                                                                
142300**********************************************                            
142400*                                                                         
142500 D-LAES-SAMMA-MARKNAD SECTION.                                            
142600*                                                                         
142700**************************************************                        
142800* ENTER TRYCKNING                                                         
142900**************************************************                        
143000     MOVE MFS-FORMATETS-ATTR TO                                           
143100                MOD-TEARTNOT-VAR-ATTR                                     
143200                MOD-TEARTNOT-BASL-ATTR                                    
143300     PERFORM IMS-GHU-PROJ-XXAP01-1                                        
143400     IF SEGMENT-FINNS                                                     
143500        IF MID-KDBASLM-ENTER NOT = ZERO                                   
143600           MOVE MID-KDBASLM-ENTER TO W-1126-KDBASLM                       
143700                                     W-KDBASLM                            
143800           PERFORM IMS-GNP-PROJ-XXAP12-UNIK-2                             
143900           IF SEGMENT-SAKNAS                                              
144000              PERFORM IMS-GNP-PROJ-XXAP12-U-NEXT-2                        
144100              IF SEGMENT-FINNS                                            
144200                 MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                   
144300              ELSE                                                        
144400                 MOVE HIGH-VALUE TO 1126-KDBASLM                          
144500              END-IF                                                      
144600           ELSE                                                           
144700              MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                      
144800           END-IF                                                         
144900           PERFORM IMS-GHU-ARTG01-3                                       
145000           PERFORM IMS-GHNP-ARTG11-UNIK-3                                 
145100           IF SEGMENT-SAKNAS                                              
145200              PERFORM IMS-GHNP-ARTG11-UNIK-NXT-3                          
145300              IF SEGMENT-FINNS                                            
145400                 CONTINUE                                                 
145500              ELSE                                                        
145600                 MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                    
145700              END-IF                                                      
145800           ELSE                                                           
145900              CONTINUE                                                    
146000           END-IF                                                         
146100        ELSE                                                              
146200           PERFORM IMS-GNP-PROJ-XXAP12-FOERSTA-2                          
146300           IF SEGMENT-FINNS                                               
146400              MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                      
146500           ELSE                                                           
146600              MOVE HIGH-VALUE TO 1126-KDBASLM                             
146700           END-IF                                                         
146800                                                                          
146900           PERFORM IMS-GHU-ARTG01-3                                       
147000           PERFORM IMS-GHNP-ARTG11-FOERSTA-3                              
147100           IF SEGMENT-FINNS                                               
147200              CONTINUE                                                    
147300           ELSE                                                           
147400              MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                       
147500           END-IF                                                         
147600        END-IF                                                            
147700     ELSE                                                                 
147800        MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                             
147900        MOVE NEJ TO UPPDATERING-SW                                        
148000        PERFORM S4-RENSA-FAELT-VISN                                       
148100        PERFORM S3-RENSA-FAELT-MOD                                        
148200     END-IF                                                               
148300     .                                                                    
148400     EJECT                                                                
148500**********************************************                            
148600*                                                                         
148700 E-KOLLA-PF8       SECTION.                                               
148800*                                                                         
148900**************************************************                        
149000     IF MID-KDBASLM-PF8 = SPACE                                           
149100        PERFORM EA-LAES                                                   
149200        IF SEGMENT-FINNS                                                  
149300           MOVE SEQC-IDARTNR TO W-IDARTNR                                 
149400                                WS-IDARTNR                                
149500                                MOD-IDARTNR-UT                            
149600                                W-IDARTNR-MIN7                            
149700                                W-IDARTNR-MIN                             
149800                                W-IDARTNR-MAX                             
149900           INSPECT MOD-IDARTNR-UT REPLACING LEADING                       
150000                       ZERO BY SPACE                                      
150100           PERFORM S6-LAES-ARTIKEL-INFO                                   
150200           IF SEGMENT-FINNS                                               
150300              PERFORM F-LAES-FOERSTA-MARKNAD                              
150400           ELSE                                                           
150500              PERFORM S3-RENSA-FAELT-MOD                                  
150600              MOVE NEJ TO UPPDATERING-SW                                  
150700              MOVE FEL-2 (SPRAK-IX) TO                                    
150800                                MOD-TEMFSFEL                              
150900           END-IF                                                         
151000        ELSE                                                              
151100**         BASEN ÄR SLUT.......                                           
151200           CONTINUE                                                       
151300           MOVE NEJ TO UPPDATERING-SW                                     
151400        END-IF                                                            
151500     ELSE                                                                 
151600        PERFORM S6-LAES-ARTIKEL-INFO                                      
151700        IF SEGMENT-FINNS                                                  
151800           PERFORM G-LAES-NAESTA                                          
151900        ELSE                                                              
152000           PERFORM S3-RENSA-FAELT-MOD                                     
152100           MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                          
152200           MOVE NEJ TO UPPDATERING-SW                                     
152300        END-IF                                                            
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700**********************************************                            
152800*                                                                         
152900 EA-LAES            SECTION.                                              
153000*                                                                         
153100**************************************************                        
153200     IF MID-IDAO-KEY = SPACE                                              
153300        MOVE WS-IDARTNR        TO W-IDARTNR                               
153400*       NÄSTA ARTIKEL > W-IDARTNR LÄSES                                   
153500        PERFORM IMS-GN-ARTNR-PROJ-ARTJ01-3                                
153600     ELSE                                                                 
153700        IF MID-IDPROJ-KEY = SPACE                                         
153800           PERFORM IMS-GN-IDAO-ARTJ01-MINMAX-3                            
153900        ELSE                                                              
154000           PERFORM IMS-GN-IDAO-IDPROJ-ARTJ01-3                            
154100        END-IF                                                            
154200     END-IF                                                               
154300     .                                                                    
154400     EJECT                                                                
154500**********************************************                            
154600*                                                                         
154700 F-LAES-FOERSTA-MARKNAD   SECTION.                                        
154800*                                                                         
154900**********************************************                            
155000* PFK-7 LÄSNING                                                           
155100*                                                                         
155200**********************************************                            
155300     PERFORM IMS-GHU-PROJ-XXAP01-1                                        
155400     IF SEGMENT-FINNS                                                     
155500        PERFORM IMS-GNP-PROJ-XXAP12-FOERSTA-2                             
155600        IF SEGMENT-FINNS                                                  
155700           MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                         
155800        ELSE                                                              
155900           MOVE HIGH-VALUE TO 1126-KDBASLM                                
156000        END-IF                                                            
156100                                                                          
156200        PERFORM IMS-GHU-ARTG01-3                                          
156300        PERFORM IMS-GHNP-ARTG11-FOERSTA-3                                 
156400        IF SEGMENT-FINNS                                                  
156500           CONTINUE                                                       
156600        ELSE                                                              
156700           MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                          
156800        END-IF                                                            
156900     ELSE                                                                 
157000        MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                             
157100        MOVE NEJ TO UPPDATERING-SW                                        
157200        PERFORM S4-RENSA-FAELT-VISN                                       
157300        PERFORM S3-RENSA-FAELT-MOD                                        
157400     END-IF                                                               
157500     .                                                                    
157600     EJECT                                                                
157700**********************************************                            
157800*                                                                         
157900 G-LAES-NAESTA       SECTION.                                             
158000*                                                                         
158100***********************************************************               
158200* PFK-8 LÄSNING                                                           
158300*                                                                         
158400***********************************************************               
158500     PERFORM IMS-GHU-PROJ-XXAP01-1                                        
158600     MOVE MID-KDBASLM-PF8 TO W-1126-KDBASLM                               
158700                             W-KDBASLM                                    
158800     PERFORM IMS-GNP-PROJ-XXAP12-UNIK-2                                   
158900     IF SEGMENT-SAKNAS                                                    
159000        PERFORM IMS-GNP-PROJ-XXAP12-U-NEXT-2                              
159100        IF SEGMENT-FINNS                                                  
159200           MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                         
159300        ELSE                                                              
159400           MOVE HIGH-VALUE TO 1126-KDBASLM                                
159500        END-IF                                                            
159600     ELSE                                                                 
159700        MOVE 1126-KDBASLM TO MOD-KDBASLM-ENTER                            
159800     END-IF                                                               
159900     PERFORM IMS-GHU-ARTG01-3                                             
160000     PERFORM IMS-GHNP-ARTG11-UNIK-3                                       
160100     IF SEGMENT-SAKNAS                                                    
160200        PERFORM IMS-GHNP-ARTG11-UNIK-NXT-3                                
160300        IF SEGMENT-FINNS                                                  
160400           CONTINUE                                                       
160500        ELSE                                                              
160600           MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                          
160700        END-IF                                                            
160800     ELSE                                                                 
160900        CONTINUE                                                          
161000     END-IF                                                               
161100     .                                                                    
161200     EJECT                                                                
161300**********************************************                            
161400*                                                                         
161500 S7-LAES-VISA-INFO-MARKNADER SECTION.                                     
161600                                                                          
161700**************************************************                        
161800* MATCHAR PROJEKTSTRUKTUREN MOT ARTIKELSTRUKTUREN.                        
161900*                                                                         
162000**************************************************                        
162100                                                                          
162200     MOVE +1       TO RAD-IX                                              
162300     MOVE +1       TO COL-IX                                              
162400     PERFORM UNTIL RAD-IX > 9                                             
162500        PERFORM UNTIL COL-IX > 4                                          
162600           IF 1126-KDBASLM = HIGH-VALUE AND                               
162700              ARTG11-ART-KDBASLM = HIGH-VALUE                             
162800              MOVE MFS-RENSA-FAELT TO                                     
162900                                   MOD-KDBASLM(RAD-IX COL-IX)             
163000                                   MOD-AFFECT(RAD-IX COL-IX)              
163100                                   MOD-KVBASLM(RAD-IX COL-IX)             
163200              ADD +1            TO COL-IX                                 
163300           ELSE                                                           
163400              IF 1126-KDBASLM < ARTG11-ART-KDBASLM                        
163500                 PERFORM S7A-VISA-1126-LAES-1126                          
163600              ELSE                                                        
163700                 IF 1126-KDBASLM > ARTG11-ART-KDBASLM                     
163800                    PERFORM S7B-VISA-ARTG11-LAES-ARTG11                   
163900                 ELSE                                                     
164000                    PERFORM S7C-VISA-AG11-LAES-AG11-1126                  
164100                 END-IF                                                   
164200              END-IF                                                      
164300           END-IF                                                         
164400        END-PERFORM                                                       
164500        MOVE +1       TO COL-IX                                           
164600        ADD  +1       TO RAD-IX                                           
164700     END-PERFORM                                                          
164800     IF SEGMENT-FINNS                                                     
164900        MOVE 1126-KDBASLM TO MOD-KDBASLM-PF8                              
165000        MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSFEL                             
165100     END-IF                                                               
165200                                                                          
165300     .                                                                    
165400     EJECT                                                                
165500**********************************************                            
165600*                                                                         
165700 S7A-VISA-1126-LAES-1126    SECTION.                                      
165800*                                                                         
165900**********************************************                            
166000                                                                          
166100     MOVE 1126-KDBASLM TO MOD-KDBASLM(RAD-IX COL-IX)                      
166200     MOVE MFS-OEPPNA-ALFA-FAELT TO                                        
166300                          MOD-AFFECT-ATTR(RAD-IX COL-IX)                  
166400     MOVE MFS-RENSA-FAELT TO                                              
166500                          MOD-AFFECT(RAD-IX COL-IX)                       
166600     ADD     +1       TO COL-IX                                           
166700     PERFORM IMS-GNP-PROJ-XXAP12-NEXT-OKV-2                               
166800     IF SEGMENT-FINNS                                                     
166900        CONTINUE                                                          
167000     ELSE                                                                 
167100        MOVE HIGH-VALUE TO 1126-KDBASLM                                   
167200     END-IF                                                               
167300     .                                                                    
167400     EJECT                                                                
167500**********************************************                            
167600*                                                                         
167700 S7B-VISA-ARTG11-LAES-ARTG11   SECTION.                                   
167800*                                                                         
167900**********************************************                            
168000                                                                          
168100     MOVE ARTG11-ART-KDBASLM TO                                           
168200                           MOD-KDBASLM(RAD-IX COL-IX)                     
168300                           WFELMARKN                                      
168400     MOVE ' SAKNAS PÅ PROJEKTBASEN "FELKVANT"' TO WFELTEXT                
168500     MOVE WFELKDBASLM        TO MOD-TEMFSINF                              
168600     IF ARTG11-ART-KVBASLM = ZERO                                         
168700        MOVE MFS-RENSA-FAELT TO                                           
168800                           MOD-KVBASLM(RAD-IX COL-IX)                     
168900     ELSE                                                                 
169000        MOVE ARTG11-ART-KVBASLM TO                                        
169100                           MOD-KVBASLM(RAD-IX COL-IX)                     
169200        INSPECT MOD-KVBASLM(RAD-IX COL-IX) REPLACING                      
169300                  LEADING  ZERO BY SPACE                                  
169400     END-IF                                                               
169500     MOVE 'J'                TO                                           
169600                          MOD-AFFECT(RAD-IX COL-IX)                       
169700     MOVE MFS-OEPPNA-ALFA-FAELT-HI TO                                     
169800                          MOD-AFFECT-ATTR(RAD-IX COL-IX)                  
169900     ADD     +1       TO COL-IX                                           
170000     PERFORM IMS-GHNP-ARTG11-OKVAL-3                                      
170100     IF SEGMENT-FINNS                                                     
170200        CONTINUE                                                          
170300     ELSE                                                                 
170400        MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                             
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800**********************************************                            
170900*                                                                         
171000 S7C-VISA-AG11-LAES-AG11-1126  SECTION.                                   
171100*                                                                         
171200**********************************************                            
171300                                                                          
171400     MOVE ARTG11-ART-KDBASLM TO                                           
171500                         MOD-KDBASLM(RAD-IX COL-IX)                       
171600     MOVE 'J'                TO                                           
171700                          MOD-AFFECT(RAD-IX COL-IX)                       
171800     MOVE MFS-OEPPNA-ALFA-FAELT-HI TO                                     
171900                          MOD-AFFECT-ATTR(RAD-IX COL-IX)                  
172000     IF 1126-IDDISTR(2) = ZERO                                            
172100        COMPUTE WS-KVBASLM = 1126-KVBLKIT *                               
172200                             ARTG11-ART-KVBASLKIT +                       
172300                             ARTG11-ART-KVBASLMD(1)                       
172400        MOVE WS-KVBASLM     TO MOD-KVBASLM(RAD-IX COL-IX)                 
172500        INSPECT MOD-KVBASLM(RAD-IX COL-IX) REPLACING LEADING              
172600                              ZERO BY SPACE                               
172700     ELSE                                                                 
172800        IF ARTG11-ART-KVBASLM = ZERO                                      
172900           MOVE MFS-RENSA-FAELT TO                                        
173000                            MOD-KVBASLM(RAD-IX COL-IX)                    
173100        ELSE                                                              
173200           MOVE ARTG11-ART-KVBASLM TO                                     
173300                            MOD-KVBASLM(RAD-IX COL-IX)                    
173400           INSPECT MOD-KVBASLM(RAD-IX COL-IX) REPLACING LEADING           
173500                              ZERO BY SPACE                               
173600        END-IF                                                            
173700     END-IF                                                               
173800     ADD     +1       TO COL-IX                                           
173900     PERFORM IMS-GHNP-ARTG11-OKVAL-3                                      
174000     IF SEGMENT-FINNS                                                     
174100        CONTINUE                                                          
174200     ELSE                                                                 
174300        MOVE HIGH-VALUE TO ARTG11-ART-KDBASLM                             
174400     END-IF                                                               
174500     PERFORM IMS-GNP-PROJ-XXAP12-NEXT-OKV-2                               
174600     IF SEGMENT-FINNS                                                     
174700        CONTINUE                                                          
174800     ELSE                                                                 
174900        MOVE HIGH-VALUE TO 1126-KDBASLM                                   
175000     END-IF                                                               
175100     .                                                                    
175200     EJECT                                                                
175300**********************************************                            
175400*                                                                         
175500 S1-ROER-EJ-FAELT-VISA SECTION.                                           
175600*                                                                         
175700*****************************************                                 
175800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-NEW                            
175900                               MOD-BEART-SVE                              
176000                               MOD-TIBASL                                 
176100                               MOD-KDPRODSL                               
176200                               MOD-IDPROJ                                 
176300                               MOD-KDERS                                  
176400                               MOD-IDARTNR-ERS                            
176500                               MOD-FLBASL                                 
176600                               MOD-IDFKNGRP                               
176700                               MOD-IDARTNR-MOTSV-UT                       
176800                                                                          
176900                               MOD-TEARTNOT-VAR                           
177000                               MOD-TEARTNOT-BASL                          
177100                                                                          
177200     MOVE +1              TO RAD-IX                                       
177300     MOVE +1              TO COL-IX                                       
177400     PERFORM UNTIL RAD-IX > 9                                             
177500        PERFORM UNTIL COL-IX > 4                                          
177600           MOVE MFS-ROER-EJ-FAELT TO MOD-KDBASLM(RAD-IX COL-IX)           
177700                                     MOD-AFFECT(RAD-IX COL-IX)            
177800                                     MOD-KVBASLM(RAD-IX COL-IX)           
177900           ADD +1                 TO COL-IX                               
178000        END-PERFORM                                                       
178100        ADD +1                    TO RAD-IX                               
178200        MOVE +1                   TO COL-IX                               
178300     END-PERFORM                                                          
178400     .                                                                    
178500     EJECT                                                                
178600**********************************************                            
178700*                                                                         
178800 S2-ROER-EJ-FAELT-IN   SECTION.                                           
178900*                                                                         
179000**************************************                                    
179100     MOVE MFS-ROER-EJ-FAELT TO  MOD-FLBASL-IN                             
179200                                MOD-IDARTNR-MOTSV-IN                      
179300                                MOD-TEARTNOT-VAR                          
179400                                MOD-TEARTNOT-BASL                         
179500                                MOD-IDARTNR-NEW                           
179600                                MOD-FLATERPAKOE-IN                        
179700                                                                          
179800     MOVE +1              TO RAD-IX                                       
179900     MOVE +1              TO COL-IX                                       
180000     PERFORM UNTIL RAD-IX > 9                                             
180100        PERFORM UNTIL COL-IX > 4                                          
180200           MOVE MFS-ROER-EJ-FAELT TO MOD-KDBASLM(RAD-IX COL-IX)           
180300                                     MOD-AFFECT(RAD-IX COL-IX)            
180400           ADD +1               TO COL-IX                                 
180500        END-PERFORM                                                       
180600        MOVE +1              TO COL-IX                                    
180700        ADD  +1              TO RAD-IX                                    
180800     END-PERFORM                                                          
180900     .                                                                    
181000     EJECT                                                                
181100**********************************************                            
181200*                                                                         
181300 S3-RENSA-FAELT-MOD SECTION.                                              
181400*                                                                         
181500*****************************************                                 
181600* RENSAR INMATNINGSFÄLTEN                                                 
181700*****************************************                                 
181800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-NEW                              
181900                             MOD-FLBASL-IN                                
182000                             MOD-FLATERPAKOE-IN                           
182100                             MOD-IDARTNR-MOTSV-IN                         
182200***                                                                       
182300                                                                          
182400     MOVE +1              TO RAD-IX                                       
182500     MOVE +1              TO COL-IX                                       
182600     PERFORM UNTIL RAD-IX > 9                                             
182700        PERFORM UNTIL COL-IX > 4                                          
182800           MOVE MFS-RENSA-FAELT TO MOD-KDBASLM(RAD-IX COL-IX)             
182900                                   MOD-AFFECT(RAD-IX COL-IX)              
183000           ADD +1               TO COL-IX                                 
183100        END-PERFORM                                                       
183200        MOVE +1              TO COL-IX                                    
183300        ADD +1               TO RAD-IX                                    
183400     END-PERFORM                                                          
183500     .                                                                    
183600     EJECT                                                                
183700**********************************************                            
183800*                                                                         
183900 S4-RENSA-FAELT-VISN SECTION.                                             
184000*                                                                         
184100*****************************************                                 
184200* RENSAR VISNINGSFÄLTEN                                                   
184300*                                                                         
184400*****************************************                                 
184500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-NEW                              
184600                             MOD-BEART-SVE                                
184700                             MOD-TIBASL                                   
184800                             MOD-KDPRODSL                                 
184900                             MOD-IDARTNR-ERS                              
185000                             MOD-FLBASL                                   
185100                             MOD-FLATERPAKOE                              
185200                             MOD-IDFKNGRP                                 
185300                             MOD-IDPROJ                                   
185400                             MOD-KDERS                                    
185500                             MOD-TEARTNOT-VAR                             
185600                             MOD-IDARTNR-MOTSV-UT                         
185700                             MOD-TEARTNOT-BASL                            
185800                             MOD-KDERS                                    
185900***                                                                       
186000                                                                          
186100     MOVE +1              TO RAD-IX                                       
186200     MOVE +1              TO COL-IX                                       
186300     PERFORM UNTIL RAD-IX > 9                                             
186400        PERFORM UNTIL COL-IX > 4                                          
186500           MOVE MFS-RENSA-FAELT TO MOD-KDBASLM(RAD-IX COL-IX)             
186600                                   MOD-AFFECT(RAD-IX COL-IX)              
186700                                   MOD-KVBASLM(RAD-IX COL-IX)             
186800           ADD +1               TO COL-IX                                 
186900        END-PERFORM                                                       
187000        MOVE +1              TO COL-IX                                    
187100        ADD +1               TO RAD-IX                                    
187200     END-PERFORM                                                          
187300     .                                                                    
187400     EJECT                                                                
187500**********************************************                            
187600*                                                                         
187700 S6-LAES-ARTIKEL-INFO      SECTION.                                       
187800*                                                                         
187900**********************************************                            
188000* PFK-7 LÄSNING                                                           
188100*                                                                         
188200**********************************************                            
188300     PERFORM IMS-GHU-ARTG01-3                                             
188400     IF SEGMENT-FINNS                                                     
188500                                                                          
188600                                                                          
188700        MOVE ARTG-ART-DABASL (3:6)  TO MOD-TIBASL                         
188800                                       WS-TIBASL                          
188900        MOVE ARTG-ART-FLBASL        TO MOD-FLBASL                         
189000        IF ARTG-ART-IDARTNR-MOTSV = ZERO                                  
189100           MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-MOTSV-UT               
189200        ELSE                                                              
189300           MOVE ARTG-ART-IDARTNR-MOTSV TO                                 
189400                                       MOD-IDARTNR-MOTSV-UT               
189500           INSPECT MOD-IDARTNR-MOTSV-UT REPLACING                         
189600                   LEADING ZERO BY SPACE                                  
189700        END-IF                                                            
189800        MOVE ARTG-ART-TEARTNOT-BASL TO MOD-TEARTNOT-BASL                  
189900        MOVE ARTG-ART-IDPROJ        TO MOD-IDPROJ                         
190000                                       WS-IDPROJ                          
190100                                       W-IDPROJ                           
190200                                       W-1123-IDPROJ                      
190300        MOVE ARTG-ART-IDAO          TO WS-IDAO                            
190400                                                                          
190500        IF MID-IDAO-KEY = SPACE AND MID-IDPROJ-KEY = SPACE                
190600           MOVE ARTG-ART-IDAO TO MOD-IDAO-KEY                             
190700           MOVE ARTG-ART-IDPROJ   TO MOD-IDPROJ-KEY                       
190800        END-IF                                                            
190900        MOVE MID-IDAO-KEY  TO W-IDAO-MIN                                  
191000        MOVE MID-IDAO-KEY  TO W-IDAO-MAX                                  
191100                                                                          
191200        PERFORM IMS-GU-ARTC01-4                                           
191300        IF SEGMENT-FINNS                                                  
191400           PERFORM S8-ARTC-IDFKNGRP-PRODSL                                
191500           PERFORM IMS-GU-ARTC01-4                                        
191600           IF ART-FLERS = JA                                              
191700              PERFORM IMS-GU-ERSB01-MINMAX-3                              
191800              IF SEGMENT-FINNS                                            
191900                 IF ERSB01-ERS-IDARTNR NOT = ZERO                         
192000                    MOVE ERSB01-ERS-IDARTNR TO W-IDARTNR                  
192100                    PERFORM IMS-GU-ARTC11-4                               
192200                    IF SEGMENT-FINNS                                      
192300                       IF CLAG-FLLSRDEL = 'N'                             
192400                         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS          
192500                       ELSE                                               
192600                          MOVE ERSB01-ERS-IDARTNR TO                      
192700                                               MOD-IDARTNR-ERS            
192800                          INSPECT MOD-IDARTNR-ERS REPLACING               
192900                                          LEADING ZERO BY SPACE           
193000                       END-IF                                             
193100                    ELSE                                                  
193200                       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS            
193300                    END-IF                                                
193400                 ELSE                                                     
193500                    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS               
193600                 END-IF                                                   
193700                 MOVE WS-IDARTNR TO W-IDARTNR                             
193800              ELSE                                                        
193900                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                  
194000              END-IF                                                      
194100           ELSE                                                           
194200              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ERS                     
194300           END-IF                                                         
194400           PERFORM IMS-GU-ARTC01-4                                        
194500           IF ART-KDERS-UTG = ZERO                                        
194600              PERFORM IMS-GNP-ARTC11-4                                    
194700              MOVE CLAG-KDERS             TO MOD-KDERS                    
194800              IF CLAG-KDUART = ' '                                        
194900                 MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                     
195000              ELSE                                                        
195100                 IF CLAG-KDUART = 'P'                                     
195200                    MOVE MED-5(SPRAK-IX)   TO MOD-TEMFSINF                
195300                 ELSE                                                     
195400                    IF CLAG-KDUART = 'M'                                  
195500                       MOVE MED-6(SPRAK-IX)   TO MOD-TEMFSINF             
195600                    ELSE                                                  
195700                       IF CLAG-KDUART = 'S'                               
195800                          MOVE MED-7(SPRAK-IX)   TO MOD-TEMFSINF          
195900                       ELSE                                               
196000                          IF CLAG-KDUART = 'A'                            
196100                           MOVE MED-8(SPRAK-IX)   TO MOD-TEMFSINF         
196200                          ELSE                                            
196300                           MOVE MFS-RENSA-FAELT   TO MOD-TEMFSINF         
196400                          END-IF                                          
196500                       END-IF                                             
196600                    END-IF                                                
196700                 END-IF                                                   
196800              END-IF                                                      
196900           ELSE                                                           
197000              MOVE ART-KDERS-UTG          TO MOD-KDERS                    
197100           END-IF                                                         
197200                                                                          
197300           MOVE 6          TO W-KDNOTTYP                                  
197400           PERFORM IMS-GHNP-NOTTYP-ARTC25-UNIK-4                          
197500           IF SEGMENT-FINNS                                               
197600              MOVE NOT-TEARTNOT        TO MOD-TEARTNOT-VAR                
197700           ELSE                                                           
197800              MOVE MFS-RENSA-FAELT     TO MOD-TEARTNOT-VAR                
197900           END-IF                                                         
198000                                                                          
198100                                                                          
198200           PERFORM IMS-GU-BENA11-BSEQ-3                                   
198300           MOVE BENA-TEXT-BEART        TO MOD-BEART-SVE                   
198400        END-IF                                                            
198500     END-IF                                                               
198600     .                                                                    
198700     EJECT                                                                
198800**********************************************                            
198900*                                                                         
199000 S8-ARTC-IDFKNGRP-PRODSL SECTION.                                         
199100*                                                                         
199200**********************************************                            
199300     MOVE ART-IDFKNGRP   TO WS-IDFKNGRP                                   
199400                            MOD-IDFKNGRP                                  
199500     MOVE ART-KDPRODSL   TO MOD-KDPRODSL                                  
199600                            WS1-KDPRODSL                                  
199700                            TEST-KDPRODSL                                 
199800     PERFORM IMS-GNP-ARTC11-4                                             
199900     MOVE CLAG-KDBPSR    TO WS-KDBPSR                                     
200000     IF KDPRODSL-UTAN-EMB                                                 
200100        MOVE 11          TO WS-KDPRODSL                                   
200200     ELSE                                                                 
200300        IF KDPRODSL-VCBV                                                  
200400           MOVE 11       TO WS-KDPRODSL                                   
200500        ELSE                                                              
200600           MOVE ZERO     TO WS-KDPRODSL                                   
200700        END-IF                                                            
200800     END-IF                                                               
200900     MOVE WS-KDPRODSL    TO W-1123-KDPRODSL                               
201000                                                                          
201100     .                                                                    
201200     EJECT                                                                
201300**********************************************                            
201400*                                                                         
201500 MFS-RENSA-MOD-IN SECTION.                                                
201600*                                                                         
201700**********************************************                            
201800                                                                          
201900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
202000                             MOD-IDARTNR-NEW                              
202100                             MOD-FLBASL-IN                                
202200                             MOD-FLATERPAKOE-IN                           
202300                             MOD-TEARTNOT-VAR                             
202400                             MOD-TEARTNOT-BASL                            
202500                             MOD-IDARTNR-MOTSV-IN                         
202600                             MOD-TEMFSFEL                                 
202700                             MOD-TEMFSINF                                 
202800                                                                          
202900     MOVE +1              TO RAD-IX                                       
203000     MOVE +1              TO COL-IX                                       
203100     PERFORM UNTIL RAD-IX > 9                                             
203200        PERFORM UNTIL COL-IX > 4                                          
203300           MOVE MFS-RENSA-FAELT TO MOD-KDBASLM(RAD-IX COL-IX)             
203400                                   MOD-AFFECT(RAD-IX COL-IX)              
203500           ADD +1               TO COL-IX                                 
203600        END-PERFORM                                                       
203700        MOVE +1              TO COL-IX                                    
203800        ADD +1               TO RAD-IX                                    
203900     END-PERFORM                                                          
204000     .                                                                    
204100     EJECT                                                                
204200**********************************************                            
204300*                                                                         
204400* IMS SEKTIONER                                                           
204500*                                                                         
204600**********************************************                            
204700     SKIP2                                                                
204800 IMS-GET-MSG SECTION.                                                     
204900                                                                          
205000     MOVE '  QC' TO GODK-STATUSKODER                                      
205100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
205200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500     SKIP2                                                                
205600 IMS-INSERT-MSG SECTION.                                                  
205700                                                                          
205800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
205900       MOVE '0' TO MFS-KDHUVOMR                                           
206000     END-IF                                                               
206100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
206200     MOVE SPACE TO GODK-STATUSKODER                                       
206300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
206400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     EJECT                                                                
206800 IMS-GHU-ARTG01-3 SECTION.                                                
206900                                                                          
207000     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
207100            DELIMITED BY SIZE INTO SSA1                                   
207200     MOVE '  GE' TO GODK-STATUSKODER                                      
207300     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA3 SSA1                    
207400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
207500     PERFORM IMS-STATUSKONTROLL                                           
207600     .                                                                    
207700     SKIP2                                                                
207800 IMS-REPL-ARTG01-3         SECTION.                                       
207900                                                                          
208000     MOVE '  ' TO GODK-STATUSKODER                                        
208100     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA3                        
208200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
208300     PERFORM IMS-STATUSKONTROLL                                           
208400     .                                                                    
208500     EJECT                                                                
208600 IMS-GHU-PROJ-XXAP01-1     SECTION.                                       
208700                                                                          
208800     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
208900            DELIMITED BY SIZE INTO SSA1                                   
209000     MOVE '  GE' TO GODK-STATUSKODER                                      
209100     CALL CBLTDLI USING GHU XXAP-PCB DLI-IO-AREA1 SSA1                    
209200     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
209300     PERFORM IMS-STATUSKONTROLL                                           
209400     .                                                                    
209500     SKIP2                                                                
209600 IMS-GNP-PROJ-XXAP12-FOERSTA-2 SECTION.                                   
209700                                                                          
209800     MOVE 'WLXXAP12*F ' TO SSA1                                           
209900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
210000     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
210100     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
210200     PERFORM IMS-STATUSKONTROLL                                           
210300     .                                                                    
210400     SKIP2                                                                
210500 IMS-GNP-PROJ-XXAP12-U-NEXT-2 SECTION.                                    
210600                                                                          
210700     STRING 'WLXXAP12(WDGXKEY >=' W-1126-KEY-X ')'                        
210800            DELIMITED BY SIZE INTO SSA1                                   
210900     MOVE '  GE' TO GODK-STATUSKODER                                      
211000     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
211100     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
211200     PERFORM IMS-STATUSKONTROLL                                           
211300     .                                                                    
211400     SKIP2                                                                
211500 IMS-GNP-PROJ-XXAP12-UNIK-2 SECTION.                                      
211600                                                                          
211700     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
211800            DELIMITED BY SIZE INTO SSA1                                   
211900     MOVE '  GE' TO GODK-STATUSKODER                                      
212000     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
212100     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
212200     PERFORM IMS-STATUSKONTROLL                                           
212300     .                                                                    
212400     EJECT                                                                
212500 IMS-GNP-PROJ-XXAP12-NEXT-OKV-2 SECTION.                                  
212600                                                                          
212700     MOVE 'WLXXAP12 ' TO SSA1                                             
212800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
212900     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
213000     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300     SKIP2                                                                
213400 IMS-GHNP-PROJ-XXAP11-OKV-1 SECTION.                                      
213500                                                                          
213600     MOVE 'WLXXAP11 ' TO SSA1                                             
213700     MOVE '  ' TO GODK-STATUSKODER                                        
213800     CALL CBLTDLI USING GHNP XXAP-PCB DLI-IO-AREA1 SSA1                   
213900     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
214000     PERFORM IMS-STATUSKONTROLL                                           
214100     .                                                                    
214200     SKIP2                                                                
214300 IMS-REPL-PROJ-XXAP11-1 SECTION.                                          
214400                                                                          
214500     MOVE '  ' TO GODK-STATUSKODER                                        
214600     CALL CBLTDLI USING REPL XXAP-PCB DLI-IO-AREA1                        
214700     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
214800     PERFORM IMS-STATUSKONTROLL                                           
214900     .                                                                    
215000     SKIP2                                                                
215100 IMS-GHNP-ARTG11-FOERSTA-3 SECTION.                                       
215200                                                                          
215300     MOVE 'WLARTG11*F ' TO SSA1                                           
215400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
215500     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA3 SSA1                   
215600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
215700     PERFORM IMS-STATUSKONTROLL                                           
215800     .                                                                    
215900     EJECT                                                                
216000 IMS-GHU-ARTG01-5 SECTION.                                                
216100                                                                          
216200     STRING 'WLARTG01(IDARTNR  =' WX-IDARTNR-X ')'                        
216300            DELIMITED BY SIZE INTO SSA1                                   
216400     MOVE '  GE' TO GODK-STATUSKODER                                      
216500     CALL CBLTDLI USING GHU ARTG-2-PCB DLI-IO-AREA5 SSA1                  
216600     MOVE ARTG-2-STATUS-CODE TO STATUS-WS                                 
216700     PERFORM IMS-STATUSKONTROLL                                           
216800     .                                                                    
216900     SKIP2                                                                
217000 IMS-GHNP-ARTG11-FOERSTA-5 SECTION.                                       
217100                                                                          
217200     MOVE 'WLARTG11*F ' TO SSA1                                           
217300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
217400     CALL CBLTDLI USING GHNP ARTG-2-PCB DLI-IO-AREA5 SSA1                 
217500     MOVE ARTG-2-STATUS-CODE TO STATUS-WS                                 
217600     PERFORM IMS-STATUSKONTROLL                                           
217700     .                                                                    
217800     SKIP2                                                                
217900 IMS-GHNP-ARTG11-OKVAL-5     SECTION.                                     
218000                                                                          
218100     MOVE 'WLARTG11 ' TO SSA1                                             
218200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
218300     CALL CBLTDLI USING GHNP ARTG-2-PCB DLI-IO-AREA5 SSA1                 
218400     MOVE ARTG-2-STATUS-CODE TO STATUS-WS                                 
218500     PERFORM IMS-STATUSKONTROLL                                           
218600     .                                                                    
218700     EJECT                                                                
218800 IMS-GHNP-ARTG11-OKVAL-3 SECTION.                                         
218900                                                                          
219000     MOVE 'WLARTG11 ' TO SSA1                                             
219100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
219200     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA3 SSA1                   
219300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
219400     PERFORM IMS-STATUSKONTROLL                                           
219500     .                                                                    
219600     SKIP2                                                                
219700 IMS-GHNP-ARTG11-UNIK-3  SECTION.                                         
219800                                                                          
219900     STRING 'WLARTG11(KDBASLM  =' W-KDBASLM-X ')'                         
220000            DELIMITED BY SIZE INTO SSA1                                   
220100     MOVE '  GE' TO GODK-STATUSKODER                                      
220200     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA3 SSA1                   
220300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
220400     PERFORM IMS-STATUSKONTROLL                                           
220500     .                                                                    
220600     SKIP2                                                                
220700 IMS-GHNP-ARTG11-NEXT-GT-3 SECTION.                                       
220800                                                                          
220900     STRING 'WLARTG11(KDBASLM  >' W-KDBASLM-X ')'                         
221000            DELIMITED BY SIZE INTO SSA1                                   
221100     MOVE '  GE' TO GODK-STATUSKODER                                      
221200     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA3 SSA1                   
221300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
221400     PERFORM IMS-STATUSKONTROLL                                           
221500     .                                                                    
221600                                                                          
221700 IMS-GHNP-ARTG11-UNIK-NXT-3 SECTION.                                      
221800                                                                          
221900     STRING 'WLARTG11(KDBASLM >=' W-KDBASLM-X ')'                         
222000            DELIMITED BY SIZE INTO SSA1                                   
222100     MOVE '  GE' TO GODK-STATUSKODER                                      
222200     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA3 SSA1                   
222300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
222400     PERFORM IMS-STATUSKONTROLL                                           
222500     .                                                                    
222600     EJECT                                                                
222700 IMS-ISRT-ARTG11-3       SECTION.                                         
222800                                                                          
222900     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
223000            DELIMITED BY SIZE INTO SSA1                                   
223100     MOVE 'WLARTG11 ' TO SSA2                                             
223200     MOVE '  ' TO GODK-STATUSKODER                                        
223300     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA3 SSA1 SSA2              
223400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
223500     PERFORM IMS-STATUSKONTROLL                                           
223600     .                                                                    
223700     SKIP2                                                                
223800 IMS-DLET-ARTG11-3       SECTION.                                         
223900                                                                          
224000     MOVE '  ' TO GODK-STATUSKODER                                        
224100     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA3                        
224200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
224300     PERFORM IMS-STATUSKONTROLL                                           
224400     .                                                                    
224500     SKIP2                                                                
224600 IMS-REPL-ARTG11-3       SECTION.                                         
224700                                                                          
224800     MOVE '  ' TO GODK-STATUSKODER                                        
224900     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA3                        
225000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
225100     PERFORM IMS-STATUSKONTROLL                                           
225200     .                                                                    
225300     SKIP2                                                                
225400 IMS-GN-IDAO-ARTJ01-MINMAX-3 SECTION.                                     
225500                                                                          
225600     STRING 'WLARTJ01(WDD2C1KY >' W-WDD2C1KY-MIN                          
225700                    '&WDD2C1KY<=' W-WDD2C1KY-MAX ')'                      
225800            DELIMITED BY SIZE INTO SSA1                                   
225900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
226000     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA3 SSA1                     
226100     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     EJECT                                                                
226500 IMS-GN-IDAO-IDPROJ-ARTJ01-3   SECTION.                                   
226600                                                                          
226700     STRING 'WLARTJ01(WDD2C1KY >' W-WDD2C1KY-MIN                          
226800                    '&WDD2C1KY<=' W-WDD2C1KY-MAX                          
226900                    '&IDPROJ   =' W-IDPROJ-X ')'                          
227000            DELIMITED BY SIZE INTO SSA1                                   
227100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
227200     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA3 SSA1                     
227300     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
227400     PERFORM IMS-STATUSKONTROLL                                           
227500     .                                                                    
227600     SKIP2                                                                
227700 IMS-GN-ARTNR-PROJ-ARTJ01-3 SECTION.                                      
227800                                                                          
227900     STRING 'WLARTJ01(IDARTNR  >' W-IDARTNR-X                             
228000                    '&IDPROJ   =' W-IDPROJ-X ')'                          
228100            DELIMITED BY SIZE INTO SSA1                                   
228200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
228300     CALL CBLTDLI USING GN ARTJ-PCB DLI-IO-AREA3 SSA1                     
228400     MOVE ARTJ-STATUS-CODE TO STATUS-WS                                   
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP2                                                                
228800 IMS-GHNP-NOTTYP-ARTC25-UNIK-4   SECTION.                                 
228900     MOVE 'WLARTC11 ' TO SSA1                                             
229000     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
229100            DELIMITED BY SIZE INTO SSA2                                   
229200     MOVE '  GE' TO GODK-STATUSKODER                                      
229300     CALL CBLTDLI USING GHNP                                              
229400                    ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                       
229500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
229600     PERFORM IMS-STATUSKONTROLL                                           
229700     .                                                                    
229800     EJECT                                                                
229900 IMS-GU-ARTC11-4  SECTION.                                                
230000                                                                          
230100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
230200            DELIMITED BY SIZE INTO SSA1                                   
230300     MOVE 'WLARTC11 ' TO SSA2                                             
230400     MOVE '  GE' TO GODK-STATUSKODER                                      
230500     CALL CBLTDLI USING GU                                                
230600                    ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                       
230700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
230800     PERFORM IMS-STATUSKONTROLL                                           
230900     .                                                                    
231000     SKIP2                                                                
231100 IMS-REPL-ARTC25-4                SECTION.                                
231200                                                                          
231300     MOVE '  ' TO GODK-STATUSKODER                                        
231400     CALL CBLTDLI USING REPL                                              
231500                    ARTC-PCB DLI-IO-AREA4                                 
231600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
231700     PERFORM IMS-STATUSKONTROLL                                           
231800     .                                                                    
231900     SKIP2                                                                
232000 IMS-ISRT-ARTC25-4              SECTION.                                  
232100                                                                          
232200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
232300            DELIMITED BY SIZE INTO SSA1                                   
232400     MOVE 'WLARTC11 '             TO SSA2                                 
232500     MOVE 'WLARTC25 '             TO SSA3                                 
232600     MOVE '  II' TO GODK-STATUSKODER                                      
232700     CALL CBLTDLI USING ISRT                                              
232800                    ARTC-PCB DLI-IO-AREA4 SSA1 SSA2 SSA3                  
232900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
233000     PERFORM IMS-STATUSKONTROLL                                           
233100     .                                                                    
233200     EJECT                                                                
233300 IMS-GU-ARTC01-4          SECTION.                                        
233400                                                                          
233500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
233600            DELIMITED BY SIZE INTO SSA1                                   
233700     MOVE '  GE' TO GODK-STATUSKODER                                      
233800     CALL CBLTDLI USING GU                                                
233900                    ARTC-PCB DLI-IO-AREA4 SSA1                            
234000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300     SKIP2                                                                
234400 IMS-GNP-ARTC11-4 SECTION.                                                
234500                                                                          
234600     MOVE 'WLARTC11 ' TO SSA1                                             
234700     MOVE '  GE' TO GODK-STATUSKODER                                      
234800     CALL CBLTDLI USING GNP                                               
234900                    ARTC-PCB DLI-IO-AREA4 SSA1                            
235000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
235100     PERFORM IMS-STATUSKONTROLL                                           
235200     .                                                                    
235300     EJECT                                                                
235400 IMS-GU-ERSB01-MINMAX-3  SECTION.                                         
235500                                                                          
235600     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
235700                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
235800            DELIMITED BY SIZE INTO SSA1                                   
235900     MOVE '  GE' TO GODK-STATUSKODER                                      
236000     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA3 SSA1                     
236100     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
236200     PERFORM IMS-STATUSKONTROLL                                           
236300     .                                                                    
236400     SKIP2                                                                
236500 IMS-GU-BENA11-BSEQ-3     SECTION.                                        
236600                                                                          
236700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
236800                               DELIMITED BY SIZE INTO SSA1                
236900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
237000                               DELIMITED BY SIZE INTO SSA2                
237100     MOVE '  ' TO GODK-STATUSKODER                                        
237200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
237300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
237400     PERFORM IMS-STATUSKONTROLL                                           
237500     .                                                                    
237600     SKIP2                                                                
237700 IMS-STATUSKONTROLL SECTION.                                              
237800                                                                          
237900     SET STATUS-IX TO 1                                                   
238000     SEARCH GODK-STATUS                                                   
238100       AT END                                                             
238200         CALL FELLOG                                                      
238300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
238400     END-SEARCH                                                           
238500     .                                                                    
238600     EJECT                                                                
238700*    -COPY WY2000P1                                                       
