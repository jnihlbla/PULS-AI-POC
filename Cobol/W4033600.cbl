000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4033600.                                                
000300 AUTHOR.         MARGARETA GABRIELSSON.                                   
000400 DATE-WRITTEN.   JULI 1986.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA SKALL INTE GE                    
000800*    NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                              
000900*                                                                         
001000*    FUNKTION.                                                            
001100*    PROGRAMMET ÄR ETT FRÅGE- OCH UPPDATERINGSPROGRAM.                    
001200*    DET UPPDATERAR FLERKOLLIUPPGIFTER SOM TIDIGARE LAGTS UPP             
001300*    MED W4033500.                                                        
001400*    FRÅGA MÅSTE FÖRST HA STÄLLTS INNAN MAN KAN UTFÖRA UPP-               
001500*    DATERING.                                                            
001600*    EFTER ATT PF4 TRYCKTS SKICKAS EN TRANS TILL W4033300                 
001700*    VILKET SKRIVER UT EN 'KOLLIFLAGGA'.                                  
001800*    UPPDATERING KAN INNEBÄRA ÄNDRING AV BÄRKOLLIUPPGIFTER,               
001900*    ÄNDRING AV INGÅENDE KOLLIS UPPGIFTER ELLER NYUPPLÄGG AV              
002000*    ETT INGÅENDE KOLLI.                                                  
002100*                                                                         
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T336                                              
002500*                     W4T336U                                             
002600*        MID:         W4I33601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        TRANSAKTION: W4T333                                              
003000*        MOD:         W4O33601                                            
003100     EJECT                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*    -- CHECKED BY WY2000                                                 
003800                                                                          
003900 77  IDPGM                        PIC X(8)   VALUE 'W4033600'.            
004000 77  JA                           PIC X(1)   VALUE 'J'.                   
004100 77  NEJ                          PIC X(1)   VALUE 'N'.                   
004200 77  WS-IDDC                      PIC X(2)   VALUE SPACE.                 
004300 77  FEL                          PIC X(1)   VALUE 'F'.                   
004400 77  INDX                         PIC S9(4)  COMP SYNC.                   
004500 77  RAD-INDX                     PIC S9(4)  VALUE +1 COMP SYNC.          
004600 77  MAX-ANTAL-ING-KOLLI-PER-SIDA PIC S9(4)  VALUE +8 COMP SYNC.          
004700 77  WS-MFS-KDMFSFOR              PIC 9.                                  
004800 77  WS-KDPRTVAL                  PIC XX     VALUE SPACE.                 
004900 77  WS-IDDISTR                   PIC X(4).                               
005000 77  WS-IDKUNDNR                  PIC X(6).                               
005100 77  WS-IDKOLLI                   PIC X(5).                               
005200 77  WS-JFR-IDKOLLI               PIC X(5).                               
005300 77  WS-IDKOLLI-FLER              PIC S9(5)  COMP-3.                      
005400 77  WS-IDKOLLI-FIRST             PIC S9(5)  COMP-3.                      
005500 77  WS-IDKOLLI-NEXT              PIC S9(5)  COMP-3.                      
005600 77  WS-IDPRODNR                  PIC S9(7)  COMP-3.                      
005700 77  PF11-TEST                    PIC X      VALUE 'N'.                   
005800*                                                                         
005900 01  WS-BEGMT.                                                            
006000     05 WS-BEGMT-RAD1                     PIC X(35).                      
006100     05 WS-BEGMT-RAD2                     PIC X(35).                      
006200 01  WS-ADGMT.                                                            
006300     05 WS-ADGMT-GATA                     PIC X(35).                      
006400     05 WS-ADGMT-PADR                     PIC X(35).                      
006500     05 WS-ADGMT-LAND                     PIC X(35).                      
006600*                                                                         
006700 77  MOD-LAENGD                   PIC S9(4)  VALUE +520 COMP SYNC.        
006800     SKIP2                                                                
006900 77  WS-SLINGA-KLAR              PIC X(1).                                
007000     88  SLINGA-KLAR                         VALUE 'J'.                   
007100     SKIP2                                                                
007200 77  WS-IDTRANS                  PIC X(4).                                
007300     88  WS-GODKAEND-BILD                    VALUE '4331' '4332'          
007400                                     '4333' '4334' '4335' '4336'          
007500                                                      '4338'.             
007600     SKIP2                                                                
007700 77  WS-VORD-FARDIGPACKAD        PIC X(1).                                
007800                                                                          
007900 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
008000 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008300       EJECT                                                              
008400 01  WS-IDPRTLST.                                                         
008500     03 WS-SYSTDEL               PIC X(1).                                
008600     03 WS-LISTTYP               PIC X(2).                                
008700     03 WS-DC                    PIC X(2).                                
008800     03 WS-KDPRT                 PIC X(3).                                
008900                                                                          
009000     EJECT                                                                
009100 01  SPAR-AREOR.                                                          
009200     03  SPAR-IDTRPTNR           PIC S9(3) COMP-3 VALUE ZERO.             
009300     03  SPAR-ADCLGEO.                                                    
009400         05  SPAR-IDDC           PIC X(2)         VALUE SPACE.            
009500         05  SPAR-ADFLGEO        PIC X(3)         VALUE SPACE.            
009600     03  SPAR-ADFLOMR            PIC S9(3) COMP-3 VALUE ZERO.             
009700     03  SPAR-ADRUTNIV           PIC S9(3) COMP-3 VALUE ZERO.             
009800     03  SPAR-DIHMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
009900     03  SPAR-DIDMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
010000     03  SPAR-ADVMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
010100     03  SPAR-ADHMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
010200     03  SPAR-DIKOLLIH           PIC S9(3) COMP-3 VALUE ZERO.             
010300     03  SPAR-DIKOLLIB           PIC S9(3) COMP-3 VALUE ZERO.             
010400     03  SPAR-DIKOLLIL           PIC S9(5) COMP-3 VALUE ZERO.             
010500     03  SPAR-FLUTLAST           PIC X            VALUE SPACE.            
010600     03  SPAR-IDDC-CROSS         PIC X(2)         VALUE SPACE.            
010700     EJECT                                                                
010800 01  WS-IDKUNDRF.                                                         
010900                                                                          
011000     03  WS-IDORDNR              PIC X(5).                                
011100     03  FILLER                  PIC X(5)  VALUE SPACE.                   
011200                                                                          
011300 01  WS-TIPACTID-8.                                                       
011400                                                                          
011500     03  WS-TIPACTID-6           PIC 9(6).                                
011600     03  FILLER                  PIC 9(2).                                
011700                                                                          
011800                                                                          
011900 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
012000                                                                          
012100     88  FEL-FUNNET                        VALUE 'J'.                     
012200     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
012300                                                                          
012400 01  KEY-SW                      PIC X.                                   
012500                                                                          
012600     88  KEY-OK                            VALUE 'J'.                     
012700                                                                          
012800                                                                          
012900 01  WS-PFK                      PIC X     VALUE SPACE.                   
013000                                                                          
013100     88  PRINTA-KOLLIFLAGGA                VALUE '4'.                     
013200     88  BLADDRA-BAKAAT                    VALUE '7'.                     
013300     88  BLADDRA-FRAMAAT                   VALUE '8'.                     
013400     EJECT                                                                
013500 01  WS-BAERKOLLI-AENDRAS        PIC X     VALUE SPACE.                   
013600                                                                          
013700     88  BAERKOLLI-AENDRAS                 VALUE 'J'.                     
013800                                                                          
013900                                                                          
014000 01  WS-BAER-KDKOLLI-AENDRAS     PIC X     VALUE SPACE.                   
014100                                                                          
014200     88  BAER-KDKOLLI-AENDRAS              VALUE 'J'.                     
014300                                                                          
014400                                                                          
014500 01  WS-ING-KOLLI-AENDRAS        PIC X     VALUE SPACE.                   
014600                                                                          
014700     88  ING-KOLLI-AENDRAS                 VALUE 'J'.                     
014800                                                                          
014900                                                                          
015000 01  WS-ING-KDKOLLI-AENDRAS     PIC X     VALUE SPACE.                    
015100                                                                          
015200     88  ING-KDKOLLI-AENDRAS              VALUE 'J'.                      
015300                                                                          
015400                                                                          
015500 01  WS-TILLAEGG-NYTT-ING-KOLLI  PIC X     VALUE SPACE.                   
015600                                                                          
015700     88  TILLAEGG-NYTT-ING-KOLLI           VALUE 'J'.                     
015800                                                                          
015900                                                                          
016000 01  WS-PLATSSOEKNING-UTFOERD    PIC X     VALUE SPACE.                   
016100                                                                          
016200     88  PLATSSOEKNING-UTFOERD             VALUE 'J'.                     
016300                                                                          
016400                                                                          
016500 01  WS-TEXTER.                                                           
016600                                                                          
016700     03  WS-ADRESS-TEXT         PIC X(7)  VALUE 'ADRESS:'.                
016800     EJECT                                                                
016900 01  WS-BAERKOLLIRAD.                                                     
017000     03  WS-KDKOLLI-BAER-IN     PIC X(8).                                 
017100     03  WS-KDEMBTYP-BAER-IN    PIC S9.                                   
017200     03  WS-DIKOLLIL-BAER-IN    PIC S9(5).                                
017300     03  WS-DIKOLLIB-BAER-IN    PIC S9(3).                                
017400     03  WS-DIKOLLIH-BAER-IN    PIC S9(3).                                
017500     03  WS-SPARAT-DATA-UR-BAS-BAER.                                      
017600         05  WS-KDKOLLID-BAER   PIC X.                                    
017700     SKIP3                                                                
017800 01  WS-ING-NY-RAD.                                                       
017900     03  WS-IDKOLLI-ING-NY  PIC S9(5).                                    
018000     03  WS-VKORDBTO-ING-NY PIC S9(6)V9.                                  
018100     03  WS-VLORDBTO-ING-NY PIC S9(4)V9(3).                               
018200     03  WS-SUORDV-KOLLI-NY PIC S9(9)V9(2).                               
018300     03  WS-SUORDV-KOLLI-NY-LOC     PIC S9(9)V9(2).                       
018400     03  WS-SUORDV-KOLLI-NY-LOCPREL PIC S9(9)V9(2).                       
018500     03  WS-KDKOLLI-ING-NY  PIC X(8).                                     
018600     03  WS-KDEMBTYP-ING-NY PIC S9.                                       
018700     03  WS-DIKOLLIL-ING-NY PIC S9(5).                                    
018800     03  WS-DIKOLLIB-ING-NY PIC S9(3).                                    
018900     03  WS-DIKOLLIH-ING-NY PIC S9(3).                                    
019000     03  WS-DATA-UR-BAS-ING-NY-KOLLI.                                     
019100         05  WS-EMBPROF-ING-NY  PIC X.                                    
019200         05  WS-KVLOCK-ING-NY   PIC S9(3).                                
019300     SKIP3                                                                
019400 01  WS-ING-IN-RAD.                                                       
019500     03  WS-IDKOLLI-ING-IN  PIC S9(5).                                    
019600     03  WS-VKORDBTO-ING-IN PIC S9(6)V9.                                  
019700     03  WS-KDKOLLI-ING-IN  PIC X(8).                                     
019800     03  WS-KDEMBTYP-ING-IN PIC S9.                                       
019900     03  WS-DIKOLLIL-ING-IN PIC S9(5).                                    
020000     03  WS-DIKOLLIB-ING-IN PIC S9(3).                                    
020100     03  WS-DIKOLLIH-ING-IN PIC S9(3).                                    
020200     EJECT                                                                
020300 01  NYCKLAR-TILL-DLI.                                                    
020400                                                                          
020500   03  W-IDGMT-X.                                                         
020600     05  W-IDDISTR-B2            PIC S9(5)   VALUE ZERO  COMP-3.          
020700     05  W-IDKUNDNR-B2           PIC S9(7)   VALUE ZERO  COMP-3.          
020800                                                                          
020900     03  W-K501-KDKOLLI-X.                                                
021000                                                                          
021100         05  W-K501-KDKOLLI      PIC X(8).                                
021200                                                                          
021300     03  W-WDE4ASEQ-X.                                                    
021400         05 W-IDDISTR            PIC S9(5)   COMP-3.                      
021500         05 W-IDKUNDNR           PIC S9(7)   COMP-3.                      
021600         05 W-IDKUNDRF.                                                   
021700             07  W-IDORDNR       PIC X(5).                                
021800             07  FILLER          PIC X(5)    VALUE SPACE.                 
021900                                                                          
022000     03  W-IDPRODNR-X.                                                    
022100         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
022200                                                                          
022300     03  W-IDKOLLI-X.                                                     
022400         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
022500                                                                          
022600     03  W-IDKOLLI-FLER-X.                                                
022700         05  W-IDKOLLI-FLER PIC S9(5)        COMP-3.                      
022800                                                                          
022900     03    W-WDQ201-X.                                                    
023000         05    W-201-IDORDER     PIC S9(7) COMP-3.                        
023100                                                                          
023200     03    W-WDQ212-X.                                                    
023300         05    W-212-IDDC        PIC  X(2).                               
023400                                                                          
023500     03  W-Q301-KEY-MIN-X.                                                
023600         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
023700         05  W-Q301-MIN-IDDC     PIC X(2).                                
023800         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
023900         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
024000                                                                          
024100     03  W-Q301-KEY-MAX-X.                                                
024200         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
024300         05  W-Q301-MAX-IDDC     PIC X(2).                                
024400         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
024500         05  FILLER              PIC X(1)    VALUE HIGH-VALUE.            
024600                                                                          
024700   03  W-IDDC-B6-X.                                                       
024800     05  W-IDDC-B6               PIC X(2).                                
024900     EJECT                                                                
025000 01  FELMEDDELANDE.                                                       
025100                                                                          
025200     03  FEL-1.                                                           
025300         05  FILLER              PIC X(40)   VALUE                        
025400             '788 FRÅGA SKALL STÄLLAS INNAN PF4/PF11  '.                  
025500         05  FILLER              PIC X(40)   VALUE                        
025600             '788 PF4/PF11 AND NEW KEYS NOT ALLOWED   '.                  
025700     03  FEL1 REDEFINES FEL-1 OCCURS 2 PIC X(40).                         
025800                                                                          
025900     03  FEL-2.                                                           
026000         05  FILLER              PIC X(40)   VALUE                        
026100             '762 ANGIVET DISTR, KUND, ORDER FINNS EJ '.                  
026200         05  FILLER              PIC X(40)   VALUE                        
026300             '762 WRONG DISTR CUST ORDER              '.                  
026400     03  FEL2 REDEFINES FEL-2 OCCURS 2 PIC X(40).                         
026500                                                                          
026600     03  FEL-3.                                                           
026700         05  FILLER              PIC X(40)   VALUE                        
026800             '749 FEL NYCKEL                          '.                  
026900         05  FILLER              PIC X(40)   VALUE                        
027000             '749 WRONG KEY                           '.                  
027100     03  FEL3 REDEFINES FEL-3 OCCURS 2 PIC X(40).                         
027200                                                                          
027300     03  FEL-4.                                                           
027400         05  FILLER              PIC X(40)   VALUE                        
027500             '726 KOLLIKOD SAKNAS                     '.                  
027600         05  FILLER              PIC X(40)   VALUE                        
027700             '726 CASE CODE MISSING                   '.                  
027800     03  FEL4 REDEFINES FEL-4 OCCURS 2 PIC X(40).                         
027900                                                                          
028000     03  FEL-5.                                                           
028100         05  FILLER              PIC X(40)   VALUE                        
028200             '724 NOLL FÅR EJ ANGES                   '.                  
028300         05  FILLER              PIC X(40)   VALUE                        
028400             '724 ZERO NOT ALLOWED                    '.                  
028500     03  FEL5 REDEFINES FEL-5 OCCURS 2 PIC X(40).                         
028600     EJECT                                                                
028700     03  FEL-6.                                                           
028800         05  FILLER              PIC X(40)   VALUE                        
028900             '748 UPPLYSTA FÄLT FEL                   '.                  
029000         05  FILLER              PIC X(40)   VALUE                        
029100             '748 HIGHLIGHTED FIELDS WRONG            '.                  
029200     03  FEL6 REDEFINES FEL-6 OCCURS 2 PIC X(40).                         
029300                                                                          
029400     03  FEL-7.                                                           
029500         05  FILLER              PIC X(40)   VALUE                        
029600             '758 KOLLI SAKNAS                        '.                  
029700         05  FILLER              PIC X(40)   VALUE                        
029800             '758 CASE MISSING                        '.                  
029900     03  FEL7 REDEFINES FEL-7 OCCURS 2 PIC X(40).                         
030000                                                                          
030100     03  FEL-8.                                                           
030200         05  FILLER              PIC X(40)   VALUE                        
030300             '824 EJ FLERKOLLI                        '.                  
030400         05  FILLER              PIC X(40)   VALUE                        
030500             '824 NOT COLLECTIVE CASES                '.                  
030600     03  FEL8 REDEFINES FEL-8 OCCURS 2 PIC X(40).                         
030700                                                                          
030800     03  FEL-9.                                                           
030900         05  FILLER              PIC X(40)   VALUE                        
031000             '717 KOLLIT EJ TIDIGARE RAPPORTERAT      '.                  
031100         05  FILLER              PIC X(40)   VALUE                        
031200             '717 CASE HAS NOT BEEN REPORTED          '.                  
031300     03  FEL9 REDEFINES FEL-9 OCCURS 2 PIC X(40).                         
031400                                                                          
031500     03  FEL-10.                                                          
031600         05  FILLER              PIC X(40)   VALUE                        
031700             '764 PLATS FINNS EJ                      '.                  
031800         05  FILLER              PIC X(40)   VALUE                        
031900             '764 LOCATION MISSING                    '.                  
032000     03  FEL10 REDEFINES FEL-10 OCCURS 2 PIC X(40).                       
032100     EJECT                                                                
032200     03  FEL-11.                                                          
032300         05  FILLER              PIC X(40)   VALUE                        
032400             '772 FELAKTIG PRINTER                    '.                  
032500         05  FILLER              PIC X(40)   VALUE                        
032600             '772 WRONG PRINTER                       '.                  
032700     03  FEL11 REDEFINES FEL-11 OCCURS 2 PIC X(40).                       
032800                                                                          
032900     03  FEL-12.                                                          
033000         05  FILLER              PIC X(40)   VALUE                        
033100             '761 ANGE MÅTT                           '.                  
033200         05  FILLER              PIC X(40)   VALUE                        
033300             '761 ENTER MEASURES                      '.                  
033400     03  FEL12 REDEFINES FEL-12 OCCURS 2 PIC X(40).                       
033500                                                                          
033600     03  FEL-13.                                                          
033700         05  FILLER              PIC X(40)   VALUE                        
033800             '828 PLATS KAN INTE AVBOKAS              '.                  
033900         05  FILLER              PIC X(40)   VALUE                        
034000             '828 LOCATION CANNOT BE CHANGED          '.                  
034100     03  FEL13 REDEFINES FEL-13 OCCURS 2 PIC X(40).                       
034200                                                                          
034300     03  FEL-14.                                                          
034400         05  FILLER              PIC X(40)   VALUE                        
034500             '812 TRYCK PF11 FÖR UPPDATERING          '.                  
034600         05  FILLER              PIC X(40)   VALUE                        
034700             '812 PRESS PF11 TO UPDATE                '.                  
034800     03  FEL14 REDEFINES FEL-14 OCCURS 2 PIC X(40).                       
034900                                                                          
035000     03  FEL-15.                                                          
035100         05  FILLER              PIC X(40)   VALUE                        
035200             '721 KOLLIT REDAN RAPPORTERAT            '.                  
035300         05  FILLER              PIC X(40)   VALUE                        
035400             '721 CASE ALREADY REPORTED               '.                  
035500     03  FEL15 REDEFINES FEL-15 OCCURS 2 PIC X(40).                       
035600     EJECT                                                                
035700 01  MEDDELANDEN.                                                         
035800     03  MED-1.                                                           
035900         05  FILLER              PIC X(61)   VALUE                        
036000             'UPPDATERING UTFÖRD                      '.                  
036100         05  FILLER              PIC X(61)   VALUE                        
036200             'UPDATING OK                             '.                  
036300     03  MED1 REDEFINES MED-1 OCCURS 2 PIC X(40).                         
036400                                                                          
036500     03  MED-2.                                                           
036600         05  FILLER              PIC X(61)   VALUE                        
036700             'KOLLIFLAGGA UTSKRIVEN                   '.                  
036800         05  FILLER              PIC X(61)   VALUE                        
036900             'CASE LABEL PRINTED                      '.                  
037000     03  MED2 REDEFINES MED-2 OCCURS 2 PIC X(40).                         
037100                                                                          
037200     03  MED-3.                                                           
037300         05  FILLER              PIC X(61)   VALUE                        
037400             'FLER KOLLI FINNS                        '.                  
037500         05  FILLER              PIC X(61)   VALUE                        
037600             'MORE CASES                              '.                  
037700     03  MED3 REDEFINES MED-3 OCCURS 2 PIC X(40).                         
037800                                                                          
037900     EJECT                                                                
038000 01    FILLER              PIC X(16)   VALUE 'EMB-TABELL'.                
038100                                                                          
038200 01    EMB-TABELL.                                                        
038300       03  EMB-TAB-X.                                                     
038400           05  KLASS OCCURS 3 INDEXED BY KL-INDX.                         
038500               07  TYP  OCCURS 5                                          
038600                        INDEXED BY TYP-INDX  PIC S9(5) COMP-3.            
038700       03  EMB-TAB REDEFINES EMB-TAB-X.                                   
038800           05  FILLER.                                                    
038900               07  PALLAR   OCCURS 5 PIC S9(5) COMP-3.                    
039000           05  FILLER.                                                    
039100               07  KRAGAR   OCCURS 5 PIC S9(5) COMP-3.                    
039200           05  FILLER.                                                    
039300               07  EMB-LOCK OCCURS 5 PIC S9(5) COMP-3.                    
039400     EJECT                                                                
039500 01  DYNAMISKA-SUBPROGRAM.                                                
039600     03  WDECEDIT            PIC X(8)  VALUE 'WDECEDIT'.                  
039700     03  FELLOG              PIC X(8)  VALUE 'FELLOG  '.                  
039800     03  CBLTDLI             PIC X(8)  VALUE 'CBLTDLI '.                  
039900     03  W006PRT             PIC X(8)  VALUE 'W006PRT '.                  
040000     03  W403PLAT            PIC X(8)  VALUE 'W403PLAT'.                  
040100     03  ABEND               PIC X(8)  VALUE 'ABEND   '.                  
040200     SKIP3                                                                
040300*01  -COPY WDECAREA                                                       
040400     EJECT                                                                
040500*                                                                         
040600                                                                          
040700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
040800*                                                                         
040900 01  FILLER                  PIC X(16)  VALUE 'LÄNKAREOR'.                
041000*                                                                         
041100 01  FILLER                  PIC X(16)  VALUE 'W006PRT  '.                
041200*   -COPY W006PRT                                                         
041300     EJECT                                                                
041400 01  FILLER                  PIC X(16)  VALUE 'W403PLAT '.                
041500*01 -COPY W403PLAT                                                        
041600     EJECT                                                                
041700*                                                                         
041800                                                                          
041900 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
042000                                                                          
042100 01    FILLER              PIC X(16)   VALUE 'MID W4I336 MID'.            
042200*01  MID -COPY W4I33601.                                                  
042300     EJECT                                                                
042400*01  -COPY WMSGAREA                                                       
042500     EJECT                                                                
042600*    03  MOD -COPY W4O33601  -RED MSG-AREA.                               
042700     EJECT                                                                
042800 01  4333-MID-IO-AREA.                                                    
042900                                                                          
043000     03  4333-MID-LL             PIC S9(4)   COMP SYNC.                   
043100     03  4333-MID-Z1             PIC X.                                   
043200     03  4333-MID-Z2             PIC X.                                   
043300     03  4333-MID-TRANSKOD       PIC X(8)    VALUE 'W4T333  '.            
043400     03  4333-MID-IDTRANS        PIC X(4)    VALUE '433F'.                
043500     03  4333-MID-KDMFSFOR       PIC X.                                   
043600*    03  MID -COPY W4I33301   -PRE 4333-.                                 
043700     EJECT                                                                
043800*01  -COPY WMFSAREA                                                       
043900     EJECT                                                                
044000 01  IMS-WS.                                                              
044100     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
044200     SKIP3                                                                
044300*                        **** STATUS-KOD FRÅN IMS                         
044400     03  STATUS-WS               PIC X(2).                                
044500         88  SEGMENT-FINNS                   VALUE '  '.                  
044600         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
044700         88  BASEN-SLUT                      VALUE 'GB'.                  
044800     SKIP3                                                                
044900     03  GODK-STATUSKODER.                                                
045000         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
045100     SKIP3                                                                
045200 01  SSA1                        PIC X(96).                               
045300 01  SSA2                        PIC X(64).                               
045400 01  SSA3                        PIC X(64).                               
045500     EJECT                                                                
045600*                            IMS FUNKTIONSKODER                           
045700*01  -COPY W0003                                                          
045800     EJECT                                                                
045900*                            DLI INPUT-OUTPUT AREA                        
046000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K501'.         
046100 01  DLI-IO-K501.                                                         
046200     03  -COPY WDK501                                                     
046300     EJECT                                                                
046400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q301'.         
046500 01  DLI-IO-Q301.                                                         
046600     03  -COPY WDQ301                                                     
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q201'.         
046900 01  DLI-IO-Q201.                                                         
047000     03  -COPY WDQ201                                                     
047100     EJECT                                                                
047200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q212'.         
047300 01  DLI-IO-Q212.                                                         
047400     03  -COPY WDQ212                                                     
047500     EJECT                                                                
047600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E401'.         
047700 01  DLI-IO-E401.                                                         
047800     03  -COPY WDE401                                                     
047900     EJECT                                                                
048000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
048100 01  DLI-IO-E601.                                                         
048200     03  -COPY WDE601                                                     
048300     EJECT                                                                
048400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
048500 01  DLI-IO-E611.                                                         
048600     03  -COPY WDE611                                                     
048700     EJECT                                                                
048800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
048900 01   DLI-IO-AREA-B601.                                                   
049000*     03  -COPY WDB601                                                    
049100     EJECT                                                                
049200 LINKAGE SECTION.                                                         
049300*01  -COPY W0009     -PRE MSG-                                            
049400     EJECT                                                                
049500*01  -COPY W0009     -PRE ALT-                                            
049600     EJECT                                                                
049700*01  -COPY W0008     -PRE WDE6-                                           
049800     05  FILLER                  PIC X.                                   
049900     EJECT                                                                
050000*01  -COPY W0008     -PRE WDE4-                                           
050100     05  FILLER                  PIC X.                                   
050200     EJECT                                                                
050300*01  -COPY W0008     -PRE WDE4E-                                          
050400     05  FILLER                  PIC X.                                   
050500     EJECT                                                                
050600*01  -COPY W0008     -PRE WDK5-                                           
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008     -PRE WDQ3-                                           
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008     -PRE ORQI-                                           
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008     -PRE WDB6-                                           
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008     -PRE PLATS-DM-                                       
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008     -PRE PLATS-DN-                                       
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008     -PRE PLATS-DP-                                       
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008     -PRE PLATS-DO-                                       
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
053300*01  -COPY W0008     -PRE PLATS-GMTC-                                     
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600*01  -COPY W0008     -PRE PLATS-WDB6-                                     
053700     05  FILLER                  PIC X.                                   
053800     EJECT                                                                
053900 PROCEDURE DIVISION USING MSG-PCB  ALT-PCB  WDE6-PCB WDE4-PCB             
054000                          WDE4E-PCB WDK5-PCB                              
054100                          WDQ3-PCB ORQI-PCB WDB6-PCB                      
054200                          PLATS-DM-PCB PLATS-DN-PCB                       
054300                          PLATS-DP-PCB PLATS-DO-PCB                       
054400                          PLATS-WDE6C-PCB PLATS-GMTC-PCB                  
054500                          PLATS-WDB6-PCB.                                 
054600                                                                          
054700     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  WDE6-PCB WDE4-PCB            
054800                           WDE4E-PCB WDK5-PCB                             
054900                           WDQ3-PCB ORQI-PCB WDB6-PCB                     
055000                           PLATS-DM-PCB PLATS-DN-PCB                      
055100                           PLATS-DP-PCB PLATS-DO-PCB                      
055200                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
055300                           PLATS-WDB6-PCB.                                
055400                                                                          
055500                                                                          
055600     PERFORM IMS-GET-MSG                                                  
055700                                                                          
055800     IF SEGMENT-FINNS                                                     
055900        PERFORM A-INITIERA                                                
056000                                                                          
056100        IF WS-GODKAEND-BILD AND KEY-OK                                    
056200           PERFORM I-HAEMTA-STARTNYCKEL                                   
056300                                                                          
056400           IF FEL-EJ-FUNNET                                               
056500              IF MID-IDDISTR-IN   = ALL '+' AND                           
056600                 MID-IDKUNDNR-IN  = ALL '+' AND                           
056700                 MID-IDORDNR-IN   = ALL '+' AND                           
056800                 MID-IDKOLLI-IN   = ALL '+'                               
056900                                                                          
057000                 EVALUATE TRUE                                            
057100                 WHEN MFS-UPDATE                                          
057200                    PERFORM B-KONTROLLER                                  
057300                    IF FEL-EJ-FUNNET                                      
057400                       PERFORM C-UPPDATERING                              
057500                       IF FEL-EJ-FUNNET                                   
057600                          MOVE MED1(INDX) TO MOD-TEMFSINF                 
057700                       END-IF                                             
057800                    END-IF                                                
057900                 WHEN PRINTA-KOLLIFLAGGA                                  
058000                    PERFORM D-KONTROLLERA-PRINTER                         
058100                    IF FEL-EJ-FUNNET                                      
058200                       PERFORM E-PRINTA-KOLLIFLAGGA                       
058300                    END-IF                                                
058400                 WHEN OTHER                                               
058500                    IF MFS-IDTRANS = '4336'                               
058600                       PERFORM G-TEST-OM-MAN-MENAT-PF11                   
058700                       IF PF11-TEST = JA                                  
058800                          MOVE FEL14(INDX) TO MOD-TEMFSFEL                
058900                          MOVE JA          TO WS-FEL-FUNNET               
059000                          PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR            
059100                       END-IF                                             
059200                    END-IF                                                
059300                 END-EVALUATE                                             
059400              ELSE                                                        
059500                                                                          
059600                 IF MFS-UPDATE  OR                                        
059700                    PRINTA-KOLLIFLAGGA                                    
059800                                                                          
059900                    IF FEL-EJ-FUNNET                                      
060000                       PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR               
060100                       MOVE FEL1(INDX) TO MOD-TEMFSFEL                    
060200                       MOVE JA TO WS-FEL-FUNNET                           
060300                    END-IF                                                
060400                 END-IF                                                   
060500              END-IF                                                      
060600                                                                          
060700              IF FEL-EJ-FUNNET                                            
060800                 PERFORM F-FRAAGA                                         
060900              END-IF                                                      
061000           ELSE                                                           
061100                                                                          
061200              IF FEL-EJ-FUNNET                                            
061300                 PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                     
061400                 MOVE FEL2(INDX) TO MOD-TEMFSFEL                          
061500                 MOVE JA TO WS-FEL-FUNNET                                 
061600              END-IF                                                      
061700           END-IF                                                         
061800        ELSE                                                              
061900                                                                          
062000           IF FEL-EJ-FUNNET                                               
062100              PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                        
062200              MOVE FEL3(INDX) TO MOD-TEMFSFEL                             
062300              MOVE JA TO WS-FEL-FUNNET                                    
062400           END-IF                                                         
062500        END-IF                                                            
062600     ELSE                                                                 
062700                                                                          
062800        PERFORM H-RENSA-NYCKLAR                                           
062900     END-IF                                                               
063000                                                                          
063100     IF FEL-EJ-FUNNET                                                     
063200        PERFORM   IMS-INSERT-MSG                                          
063300        MOVE ZERO TO RETURN-CODE                                          
063400     ELSE                                                                 
063500        MOVE SPACE TO MOD-TEMFSINF                                        
063600        PERFORM   IMS-ROLLBACK                                            
063700        PERFORM   IMS-INSERT-MSG                                          
063800        MOVE ZERO TO RETURN-CODE                                          
063900     END-IF                                                               
064000                                                                          
064100     GOBACK                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 A-INITIERA SECTION.                                                      
064500                                                                          
064600                                                                          
064700     IF MSG-DUBBLA-TRANSKODER                                             
064800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33601               
064900         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
065000         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
065100         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
065200         MOVE MSG-IDPFK TO MFS-IDPFK WS-PFK                               
065300     ELSE                                                                 
065400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33601                
065500         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
065600         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
065700         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK WS-PFK                  
065800     END-IF                                                               
065900     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
066000                                                                          
066100     IF MID-IDDISTR-IN = ALL '+'                                          
066200         MOVE MID-IDDISTR-UT TO WS-IDDISTR                                
066300         INSPECT WS-IDDISTR REPLACING ALL  SPACE BY ZERO                  
066400     ELSE                                                                 
066500         MOVE MID-IDDISTR-IN TO WS-IDDISTR                                
066600     END-IF                                                               
066700                                                                          
066800     IF MID-IDKUNDNR-IN = ALL '+'                                         
066900         MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                              
067000         INSPECT WS-IDKUNDNR REPLACING ALL  SPACE BY ZERO                 
067100     ELSE                                                                 
067200         MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                              
067300     END-IF                                                               
067400                                                                          
067500     IF MID-IDORDNR-IN = ALL '+'                                          
067600         MOVE MID-IDORDNR-UT TO WS-IDORDNR                                
067700         INSPECT WS-IDORDNR REPLACING ALL  SPACE BY ZERO                  
067800     ELSE                                                                 
067900         MOVE MID-IDORDNR-IN TO WS-IDORDNR                                
068000     END-IF                                                               
068100                                                                          
068200     IF MID-IDKOLLI-IN = ALL '+'                                          
068300         MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                                
068400         INSPECT WS-IDKOLLI REPLACING ALL  SPACE BY ZERO                  
068500     ELSE                                                                 
068600         MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                                
068700     END-IF                                                               
068800                                                                          
068900     IF MID-IDDC-IN = ALL '+'                                             
069000       MOVE MID-IDDC-UT                   TO WS-IDDC                      
069100     ELSE                                                                 
069200       MOVE MID-IDDC-IN                   TO WS-IDDC                      
069300     END-IF                                                               
069400                                                                          
069500     IF WS-IDDC IS > SPACE                                                
069600       MOVE JA                            TO KEY-SW                       
069700     ELSE                                                                 
069800       MOVE NEJ                           TO KEY-SW                       
069900     END-IF                                                               
070000                                                                          
070100     IF MID-KDPRTVAL-IN = ALL '+' OR SPACE OR '0 '                        
070200         IF MID-KDPRTVAL-UT = ALL '+' OR '0 '                             
070300             MOVE SPACE TO WS-KDPRTVAL                                    
070400         ELSE                                                             
070500             MOVE MID-KDPRTVAL-UT TO WS-KDPRTVAL                          
070600         END-IF                                                           
070700     ELSE                                                                 
070800         MOVE MID-KDPRTVAL-IN TO WS-KDPRTVAL                              
070900     END-IF                                                               
071000                                                                          
071100     IF MFS-IDTRANS = '4336'                                              
071200       MOVE MID-IDPRODNR        TO WS-IDPRODNR                            
071300       MOVE MID-IDKOLLI-FLER    TO WS-IDKOLLI-FLER                        
071400       MOVE MID-IDKOLLI-FIRST   TO WS-IDKOLLI-FIRST                       
071500       MOVE MID-IDKOLLI-NEXT    TO WS-IDKOLLI-NEXT                        
071600     ELSE                                                                 
071700       MOVE ZERO                TO WS-IDPRODNR                            
071800       MOVE ZERO                TO WS-IDKOLLI-FLER                        
071900       MOVE ZERO                TO WS-IDKOLLI-FIRST                       
072000       MOVE ZERO                TO WS-IDKOLLI-NEXT                        
072100     END-IF                                                               
072200                                                                          
072300     MOVE LOW-VALUE           TO MSG-AREA                                 
072400     MOVE 'W4O336N1'          TO MFS-IDMOD                                
072500     MOVE '4336'              TO MOD-IDTRANS                              
072600     MOVE MOD-LAENGD          TO MSG-KVLL                                 
072700                                                                          
072800     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
072900     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
073000                                                                          
073100     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
073200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
073300                                                                          
073400     MOVE WS-IDORDNR TO MOD-IDORDNR-UT                                    
073500     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
073600                                                                          
073700     MOVE WS-IDKOLLI TO MOD-IDKOLLI-UT                                    
073800     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
073900                                                                          
074000     MOVE WS-KDPRTVAL TO MOD-KDPRTVAL-UT                                  
074100     MOVE WS-IDDC     TO MOD-IDDC-UT                                      
074200                                                                          
074300     INSPECT MID-ADFLOMR REPLACING LEADING SPACE BY ZERO                  
074400     INSPECT MID-ADRUTNIV REPLACING LEADING SPACE BY ZERO                 
074500     INSPECT MID-ADVMODUL REPLACING LEADING SPACE BY ZERO                 
074600                                                                          
074700     IF SWEDISH-TEXT                                                      
074800         MOVE +1 TO INDX                                                  
074900     ELSE                                                                 
075000         MOVE +2 TO INDX                                                  
075100     END-IF                                                               
075200                                                                          
075300     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
075400                                   MOD-IDKUNDNR-IN                        
075500                                   MOD-IDORDNR-IN                         
075600                                   MOD-IDKOLLI-IN                         
075700                                   MOD-KDPRTVAL-IN                        
075800                                   MOD-KDKOLLI-BAER-IN                    
075900                                   MOD-KDKOLLI-BAER-UT                    
076000                                   MOD-KDEMBTYP-BAER-IN                   
076100                                   MOD-KDEMBTYP-BAER-UT                   
076200                                   MOD-DIKOLLIL-BAER-IN                   
076300                                   MOD-DIKOLLIL-BAER-UT                   
076400                                   MOD-DIKOLLIB-BAER-IN                   
076500                                   MOD-DIKOLLIB-BAER-UT                   
076600                                   MOD-DIKOLLIH-BAER-IN                   
076700                                   MOD-DIKOLLIH-BAER-UT                   
076800                                   MOD-KDCMD                              
076900                                   MOD-IDKOLLI-ING-NY                     
077000                                   MOD-VKORDBTO-ING-NY                    
077100                                   MOD-KDKOLLI-ING-NY                     
077200                                   MOD-KDEMBTYP-ING-NY                    
077300                                   MOD-DIKOLLIL-ING-NY                    
077400                                   MOD-DIKOLLIB-ING-NY                    
077500                                   MOD-DIKOLLIH-ING-NY                    
077600                                   MOD-ADRESS-TEXT                        
077700                                   MOD-ADFLGEO                            
077800                                   MOD-ADFLOMR                            
077900                                   MOD-ADRUTNIV                           
078000                                   MOD-ADVMODUL                           
078100                                   MOD-TEMFSFEL                           
078200                                   MOD-TEMFSINF                           
078300     MOVE +1 TO RAD-INDX                                                  
078400     PERFORM UNTIL RAD-INDX > MAX-ANTAL-ING-KOLLI-PER-SIDA                
078500         MOVE MFS-RENSA-FAELT   TO MOD-IDKOLLI-ING-UT(RAD-INDX)           
078600                                   MOD-VKORDBTO-ING-UT(RAD-INDX)          
078700                                   MOD-KDKOLLI-ING-UT(RAD-INDX)           
078800                                   MOD-KDEMBTYP-ING-UT(RAD-INDX)          
078900                                   MOD-DIKOLLIL-ING-UT(RAD-INDX)          
079000                                   MOD-DIKOLLIB-ING-UT(RAD-INDX)          
079100                                   MOD-DIKOLLIH-ING-UT(RAD-INDX)          
079200         ADD +1 TO RAD-INDX                                               
079300     END-PERFORM                                                          
079400                                                                          
079500     MOVE NEJ       TO WS-FEL-FUNNET                                      
079600     .                                                                    
079700     EJECT                                                                
079800 B-KONTROLLER SECTION.                                                    
079900                                                                          
080000     PERFORM S01-NOLLSTAELL                                               
080100                                                                          
080200     IF MID-KDKOLLI-BAER-IN = ALL '+' AND                                 
080300        MID-KDEMBTYP-BAER-IN = ALL '+' AND                                
080400        MID-DIKOLLIL-BAER-IN = ALL '+' AND                                
080500        MID-DIKOLLIB-BAER-IN = ALL '+' AND                                
080600        MID-DIKOLLIH-BAER-IN = ALL '+'                                    
080700        CONTINUE                                                          
080800****       INGENTING ÄNDRAS FÖR BÄRKOLLIT                                 
080900     ELSE                                                                 
081000                                                                          
081100       PERFORM BA-KONTROLLERA-BAERKOLLI                                   
081200     END-IF                                                               
081300                                                                          
081400     IF MID-KDCMD NOT = ALL '+'                                           
081500       IF MID-KDCMD = 'N' OR 'Ä' OR ' '                                   
081600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR                      
081700                                                                          
081800         IF MID-KDCMD = 'N' OR 'Ä'                                        
081900           IF MID-KDCMD = 'Ä'                                             
082000             PERFORM BB-KONTROLLERA-ING-KOLLI                             
082100           ELSE                                                           
082200             PERFORM BC-KONTROLLERA-NYTT-ING-KOLLI                        
082300           END-IF                                                         
082400         END-IF                                                           
082500                                                                          
082600       ELSE                                                               
082700         IF FEL-EJ-FUNNET                                                 
082800           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
082900           MOVE FEL6(INDX) TO MOD-TEMFSFEL                                
083000           MOVE JA TO WS-FEL-FUNNET                                       
083100         END-IF                                                           
083200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR                        
083300       END-IF                                                             
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 BA-KONTROLLERA-BAERKOLLI SECTION.                                        
083800                                                                          
083900     IF MID-KDKOLLI-BAER-IN = ALL '+' OR SPACE                            
084000       CONTINUE                                                           
084100     ELSE                                                                 
084200      MOVE MID-KDKOLLI-BAER-IN  TO W-K501-KDKOLLI                         
084300      MOVE JA                   TO WS-BAERKOLLI-AENDRAS                   
084400      MOVE JA                   TO WS-BAER-KDKOLLI-AENDRAS                
084500      PERFORM IMS-GU-K501-KVAL                                            
084600                                                                          
084700      IF SEGMENT-SAKNAS                                                   
084800       IF FEL-EJ-FUNNET                                                   
084900        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
085000        MOVE FEL4(INDX)   TO MOD-TEMFSFEL                                 
085100        MOVE JA           TO WS-FEL-FUNNET                                
085200       END-IF                                                             
085300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-BAER-IN-ATTR                
085400      ELSE                                                                
085500       MOVE EMB-KDKOLLI      TO WS-KDKOLLI-BAER-IN                        
085600       MOVE EMB-KDKOLLID     TO WS-KDKOLLID-BAER                          
085700      END-IF                                                              
085800     END-IF                                                               
085900                                                                          
086000     EVALUATE TRUE                                                        
086100     WHEN MID-KDEMBTYP-BAER-IN NOT = ALL '+'                              
086200      MOVE JA                   TO WS-BAERKOLLI-AENDRAS                   
086300      INSPECT MID-KDEMBTYP-BAER-IN REPLACING                              
086400                          LEADING SPACE BY ZERO                           
086500      IF MID-KDEMBTYP-BAER-IN NUMERIC                                     
086600       IF MID-KDEMBTYP-BAER-IN = ZERO                                     
086700        IF FEL-EJ-FUNNET                                                  
086800         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
086900         MOVE FEL5(INDX)    TO MOD-TEMFSFEL                               
087000         MOVE JA            TO WS-FEL-FUNNET                              
087100        END-IF                                                            
087200        MOVE MFS-NUM-FAELT-FEL TO                                         
087300                      MOD-KDEMBTYP-BAER-IN-ATTR                           
087400       ELSE                                                               
087500        MOVE MID-KDEMBTYP-BAER-IN  TO WS-KDEMBTYP-BAER-IN                 
087600       END-IF                                                             
087700      ELSE                                                                
087800        IF FEL-EJ-FUNNET                                                  
087900          PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                            
088000          MOVE FEL6(INDX)        TO MOD-TEMFSFEL                          
088100          MOVE JA                TO WS-FEL-FUNNET                         
088200        END-IF                                                            
088300        MOVE MFS-NUM-FAELT-FEL   TO                                       
088400             MOD-KDEMBTYP-BAER-IN-ATTR                                    
088500      END-IF                                                              
088600     WHEN BAER-KDKOLLI-AENDRAS                                            
088700       IF SEGMENT-FINNS                                                   
088800         MOVE EMB-KDEMBTYP TO WS-KDEMBTYP-BAER-IN                         
088900       END-IF                                                             
089000     END-EVALUATE                                                         
089100                                                                          
089200     EVALUATE TRUE                                                        
089300     WHEN MID-DIKOLLIL-BAER-IN NOT = ALL '+'                              
089400      MOVE JA                   TO WS-BAERKOLLI-AENDRAS                   
089500      INSPECT MID-DIKOLLIL-BAER-IN                                        
089600                          REPLACING LEADING SPACE BY ZERO                 
089700      IF MID-DIKOLLIL-BAER-IN NUMERIC                                     
089800       IF MID-DIKOLLIL-BAER-IN = ZERO                                     
089900        IF FEL-EJ-FUNNET                                                  
090000         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
090100         MOVE FEL5(INDX)    TO MOD-TEMFSFEL                               
090200         MOVE JA            TO WS-FEL-FUNNET                              
090300        END-IF                                                            
090400        MOVE MFS-NUM-FAELT-FEL TO                                         
090500                      MOD-DIKOLLIL-BAER-IN-ATTR                           
090600       ELSE                                                               
090700        MOVE MID-DIKOLLIL-BAER-IN TO WS-DIKOLLIL-BAER-IN                  
090800       END-IF                                                             
090900      ELSE                                                                
091000       IF FEL-EJ-FUNNET                                                   
091100        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
091200        MOVE FEL6(INDX)        TO MOD-TEMFSFEL                            
091300        MOVE JA                TO WS-FEL-FUNNET                           
091400       END-IF                                                             
091500       MOVE MFS-NUM-FAELT-FEL     TO                                      
091600                         MOD-DIKOLLIL-BAER-IN-ATTR                        
091700      END-IF                                                              
091800     WHEN BAER-KDKOLLI-AENDRAS                                            
091900       IF SEGMENT-FINNS                                                   
092000         MOVE EMB-DIKOLLIL TO WS-DIKOLLIL-BAER-IN                         
092100       END-IF                                                             
092200     END-EVALUATE                                                         
092300                                                                          
092400     EVALUATE TRUE                                                        
092500     WHEN MID-DIKOLLIB-BAER-IN NOT = ALL '+'                              
092600      MOVE JA                   TO WS-BAERKOLLI-AENDRAS                   
092700      INSPECT MID-DIKOLLIB-BAER-IN REPLACING LEADING SPACE BY ZERO        
092800      IF MID-DIKOLLIB-BAER-IN NUMERIC                                     
092900       IF MID-DIKOLLIB-BAER-IN = ZERO                                     
093000        IF FEL-EJ-FUNNET                                                  
093100         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
093200         MOVE FEL5(INDX)    TO MOD-TEMFSFEL                               
093300         MOVE JA            TO WS-FEL-FUNNET                              
093400        END-IF                                                            
093500        MOVE MFS-NUM-FAELT-FEL TO                                         
093600                       MOD-DIKOLLIB-BAER-IN-ATTR                          
093700       ELSE                                                               
093800        MOVE MID-DIKOLLIB-BAER-IN TO WS-DIKOLLIB-BAER-IN                  
093900       END-IF                                                             
094000      ELSE                                                                
094100       IF FEL-EJ-FUNNET                                                   
094200        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
094300        MOVE FEL6(INDX)        TO MOD-TEMFSFEL                            
094400        MOVE JA                TO WS-FEL-FUNNET                           
094500       END-IF                                                             
094600       MOVE MFS-NUM-FAELT-FEL     TO                                      
094700                          MOD-DIKOLLIB-BAER-IN-ATTR                       
094800      END-IF                                                              
094900     WHEN BAER-KDKOLLI-AENDRAS                                            
095000       IF SEGMENT-FINNS                                                   
095100         MOVE EMB-DIKOLLIB TO WS-DIKOLLIB-BAER-IN                         
095200       END-IF                                                             
095300     END-EVALUATE                                                         
095400                                                                          
095500     EVALUATE TRUE                                                        
095600     WHEN MID-DIKOLLIH-BAER-IN NOT = ALL '+'                              
095700      MOVE JA                   TO WS-BAERKOLLI-AENDRAS                   
095800      INSPECT MID-DIKOLLIH-BAER-IN REPLACING LEADING SPACE BY ZERO        
095900      IF MID-DIKOLLIH-BAER-IN NUMERIC                                     
096000       IF MID-DIKOLLIH-BAER-IN = ZERO                                     
096100        IF FEL-EJ-FUNNET                                                  
096200         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
096300         MOVE FEL5(INDX)    TO MOD-TEMFSFEL                               
096400         MOVE JA            TO WS-FEL-FUNNET                              
096500        END-IF                                                            
096600        MOVE MFS-NUM-FAELT-FEL TO                                         
096700                        MOD-DIKOLLIH-BAER-IN-ATTR                         
096800       ELSE                                                               
096900        MOVE MID-DIKOLLIH-BAER-IN TO WS-DIKOLLIH-BAER-IN                  
097000       END-IF                                                             
097100      ELSE                                                                
097200       IF FEL-EJ-FUNNET                                                   
097300        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
097400        MOVE FEL6(INDX)        TO MOD-TEMFSFEL                            
097500        MOVE JA                TO WS-FEL-FUNNET                           
097600       END-IF                                                             
097700       MOVE MFS-NUM-FAELT-FEL     TO                                      
097800                           MOD-DIKOLLIH-BAER-IN-ATTR                      
097900      END-IF                                                              
098000     WHEN BAER-KDKOLLI-AENDRAS                                            
098100       IF SEGMENT-FINNS                                                   
098200         MOVE EMB-DIKOLLIH TO WS-DIKOLLIH-BAER-IN                         
098300       END-IF                                                             
098400     END-EVALUATE                                                         
098500     .                                                                    
098600     EJECT                                                                
098700 BB-KONTROLLERA-ING-KOLLI SECTION.                                        
098800                                                                          
098900     IF MID-IDKOLLI-ING-NY = ALL '+'                                      
099000                                                                          
099100       IF FEL-EJ-FUNNET                                                   
099200         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
099300         MOVE FEL7(INDX) TO MOD-TEMFSFEL                                  
099400         MOVE JA            TO WS-FEL-FUNNET                              
099500       END-IF                                                             
099600       MOVE MFS-NUM-FAELT-FEL TO                                          
099700                    MOD-IDKOLLI-ING-NY-ATTR                               
099800     ELSE                                                                 
099900                                                                          
100000       INSPECT MID-IDKOLLI-ING-NY                                         
100100                   REPLACING LEADING SPACE BY ZERO                        
100200                                                                          
100300       MOVE +1 TO RAD-INDX                                                
100400       PERFORM UNTIL RAD-INDX > MAX-ANTAL-ING-KOLLI-PER-SIDA              
100500         INSPECT MID-IDKOLLI-ING-UT (RAD-INDX)                            
100600                     REPLACING LEADING SPACE BY ZERO                      
100700         ADD +1 TO RAD-INDX                                               
100800       END-PERFORM                                                        
100900                                                                          
101000       IF MID-IDKOLLI-ING-NY NUMERIC                                      
101100         MOVE +1 TO RAD-INDX                                              
101200         PERFORM UNTIL RAD-INDX > MAX-ANTAL-ING-KOLLI-PER-SIDA            
101300          OR MID-IDKOLLI-ING-NY =                                         
101400             MID-IDKOLLI-ING-UT (RAD-INDX)                                
101500           ADD +1 TO RAD-INDX                                             
101600         END-PERFORM                                                      
101700         IF MID-IDKOLLI-ING-NY = MID-IDKOLLI-ING-UT(RAD-INDX)             
101800           MOVE MID-IDKOLLI-ING-NY TO                                     
101900                             WS-IDKOLLI-ING-IN                            
102000         ELSE                                                             
102100           IF FEL-EJ-FUNNET                                               
102200            PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                          
102300            MOVE FEL9(INDX) TO MOD-TEMFSFEL                               
102400            MOVE JA           TO WS-FEL-FUNNET                            
102500           END-IF                                                         
102600           MOVE MFS-NUM-FAELT-FEL TO                                      
102700                         MOD-IDKOLLI-ING-NY-ATTR                          
102800         END-IF                                                           
102900       ELSE                                                               
103000         IF FEL-EJ-FUNNET                                                 
103100          PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                            
103200          MOVE FEL6(INDX) TO MOD-TEMFSFEL                                 
103300          MOVE JA           TO WS-FEL-FUNNET                              
103400         END-IF                                                           
103500         MOVE MFS-NUM-FAELT-FEL TO                                        
103600                       MOD-IDKOLLI-ING-NY-ATTR                            
103700       END-IF                                                             
103800                                                                          
103900       IF FEL-EJ-FUNNET                                                   
104000                                                                          
104100         IF MID-KDKOLLI-ING-NY = ALL '+' OR SPACE                         
104200           CONTINUE                                                       
104300         ELSE                                                             
104400           MOVE MID-KDKOLLI-ING-NY  TO W-K501-KDKOLLI                     
104500           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
104600           MOVE JA                   TO WS-ING-KDKOLLI-AENDRAS            
104700                                                                          
104800           PERFORM IMS-GU-K501-KVAL                                       
104900                                                                          
105000           IF SEGMENT-SAKNAS                                              
105100             IF FEL-EJ-FUNNET                                             
105200              PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                        
105300              MOVE FEL4(INDX)   TO MOD-TEMFSFEL                           
105400              MOVE JA           TO WS-FEL-FUNNET                          
105500             END-IF                                                       
105600             MOVE MFS-ALFA-FAELT-FEL TO                                   
105700                           MOD-KDKOLLI-ING-NY-ATTR                        
105800           ELSE                                                           
105900             MOVE EMB-KDKOLLI      TO WS-KDKOLLI-ING-IN                   
106000           END-IF                                                         
106100         END-IF                                                           
106200                                                                          
106300                                                                          
106400         EVALUATE TRUE                                                    
106500         WHEN MID-KDEMBTYP-ING-NY NOT = ALL '+'                           
106600           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
106700           INSPECT MID-KDEMBTYP-ING-NY REPLACING                          
106800                               LEADING SPACE BY ZERO                      
106900           IF MID-KDEMBTYP-ING-NY NUMERIC                                 
107000             IF MID-KDEMBTYP-ING-NY = ZERO                                
107100               IF FEL-EJ-FUNNET                                           
107200                 PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                     
107300                 MOVE FEL5(INDX)    TO MOD-TEMFSFEL                       
107400                 MOVE JA            TO WS-FEL-FUNNET                      
107500               END-IF                                                     
107600               MOVE MFS-NUM-FAELT-FEL TO                                  
107700                             MOD-KDEMBTYP-ING-NY-ATTR                     
107800             ELSE                                                         
107900              MOVE MID-KDEMBTYP-ING-NY TO                                 
108000                                 WS-KDEMBTYP-ING-IN                       
108100             END-IF                                                       
108200           ELSE                                                           
108300             IF FEL-EJ-FUNNET                                             
108400               PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                       
108500               MOVE FEL6(INDX)        TO MOD-TEMFSFEL                     
108600               MOVE JA                TO WS-FEL-FUNNET                    
108700             END-IF                                                       
108800             MOVE MFS-NUM-FAELT-FEL     TO                                
108900                               MOD-KDEMBTYP-ING-NY-ATTR                   
109000           END-IF                                                         
109100         WHEN ING-KDKOLLI-AENDRAS                                         
109200           IF SEGMENT-FINNS                                               
109300             MOVE EMB-KDEMBTYP TO WS-KDEMBTYP-ING-IN                      
109400           END-IF                                                         
109500         END-EVALUATE                                                     
109600                                                                          
109700         EVALUATE TRUE                                                    
109800                                                                          
109900         WHEN MID-DIKOLLIL-ING-NY NOT = ALL '+'                           
110000           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
110100           INSPECT MID-DIKOLLIL-ING-NY                                    
110200                               REPLACING LEADING SPACE BY ZERO            
110300           IF MID-DIKOLLIL-ING-NY NUMERIC                                 
110400             IF MID-DIKOLLIL-ING-NY = ZERO                                
110500               IF FEL-EJ-FUNNET                                           
110600                 PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                     
110700                 MOVE FEL5(INDX)    TO MOD-TEMFSFEL                       
110800                 MOVE JA            TO WS-FEL-FUNNET                      
110900               END-IF                                                     
111000               MOVE MFS-NUM-FAELT-FEL TO                                  
111100                             MOD-DIKOLLIL-ING-NY-ATTR                     
111200             ELSE                                                         
111300               MOVE MID-DIKOLLIL-ING-NY TO                                
111400                                  WS-DIKOLLIL-ING-IN                      
111500             END-IF                                                       
111600           ELSE                                                           
111700             IF FEL-EJ-FUNNET                                             
111800               PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                       
111900               MOVE FEL6(INDX)        TO MOD-TEMFSFEL                     
112000               MOVE JA                TO WS-FEL-FUNNET                    
112100             END-IF                                                       
112200             MOVE MFS-NUM-FAELT-FEL   TO                                  
112300                                 MOD-DIKOLLIL-ING-NY-ATTR                 
112400           END-IF                                                         
112500         WHEN ING-KDKOLLI-AENDRAS                                         
112600           IF SEGMENT-FINNS                                               
112700             MOVE EMB-DIKOLLIL TO WS-DIKOLLIL-ING-IN                      
112800           END-IF                                                         
112900         END-EVALUATE                                                     
113000                                                                          
113100         EVALUATE TRUE                                                    
113200         WHEN MID-DIKOLLIB-ING-NY NOT = ALL '+'                           
113300           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
113400           INSPECT MID-DIKOLLIB-ING-NY                                    
113500                        REPLACING LEADING SPACE BY ZERO                   
113600           IF MID-DIKOLLIB-ING-NY NUMERIC                                 
113700             IF MID-DIKOLLIB-ING-NY = ZERO                                
113800               IF FEL-EJ-FUNNET                                           
113900                 PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                     
114000                 MOVE FEL5(INDX)    TO MOD-TEMFSFEL                       
114100                 MOVE JA            TO WS-FEL-FUNNET                      
114200               END-IF                                                     
114300               MOVE MFS-NUM-FAELT-FEL TO                                  
114400                              MOD-DIKOLLIB-ING-NY-ATTR                    
114500             ELSE                                                         
114600               MOVE MID-DIKOLLIB-ING-NY TO                                
114700                                 WS-DIKOLLIB-ING-IN                       
114800             END-IF                                                       
114900           ELSE                                                           
115000             IF FEL-EJ-FUNNET                                             
115100              PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                        
115200              MOVE FEL6(INDX)        TO MOD-TEMFSFEL                      
115300              MOVE JA                TO WS-FEL-FUNNET                     
115400             END-IF                                                       
115500             MOVE MFS-NUM-FAELT-FEL     TO                                
115600                                MOD-DIKOLLIB-ING-NY-ATTR                  
115700           END-IF                                                         
115800         WHEN ING-KDKOLLI-AENDRAS                                         
115900           IF SEGMENT-FINNS                                               
116000             MOVE EMB-DIKOLLIB TO WS-DIKOLLIB-ING-IN                      
116100           END-IF                                                         
116200         END-EVALUATE                                                     
116300                                                                          
116400         EVALUATE TRUE                                                    
116500         WHEN MID-DIKOLLIH-ING-NY NOT = ALL '+'                           
116600           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
116700           INSPECT MID-DIKOLLIH-ING-NY                                    
116800                         REPLACING LEADING SPACE BY ZERO                  
116900           IF MID-DIKOLLIH-ING-NY NUMERIC                                 
117000             IF MID-DIKOLLIH-ING-NY = ZERO                                
117100               IF FEL-EJ-FUNNET                                           
117200                 PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                     
117300                 MOVE FEL5(INDX)    TO MOD-TEMFSFEL                       
117400                 MOVE JA            TO WS-FEL-FUNNET                      
117500               END-IF                                                     
117600               MOVE MFS-NUM-FAELT-FEL TO                                  
117700                               MOD-DIKOLLIH-ING-NY-ATTR                   
117800             ELSE                                                         
117900               MOVE MID-DIKOLLIH-ING-NY TO                                
118000                             WS-DIKOLLIH-ING-IN                           
118100             END-IF                                                       
118200           ELSE                                                           
118300             IF FEL-EJ-FUNNET                                             
118400               PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                       
118500               MOVE FEL6(INDX)        TO MOD-TEMFSFEL                     
118600               MOVE JA                TO WS-FEL-FUNNET                    
118700             END-IF                                                       
118800             MOVE MFS-NUM-FAELT-FEL     TO                                
118900                                 MOD-DIKOLLIH-ING-NY-ATTR                 
119000           END-IF                                                         
119100         WHEN ING-KDKOLLI-AENDRAS                                         
119200           IF SEGMENT-FINNS                                               
119300             MOVE EMB-DIKOLLIH TO WS-DIKOLLIH-ING-IN                      
119400           END-IF                                                         
119500         END-EVALUATE                                                     
119600                                                                          
119700                                                                          
119800                                                                          
119900         IF MID-VKORDBTO-ING-NY NOT = ALL '+'                             
120000           MOVE JA                   TO WS-ING-KOLLI-AENDRAS              
120100           MOVE MID-VKORDBTO-ING-NY TO DEC-IDFRIDATA                      
120200           MOVE +6                        TO DEC-KVHELTAL                 
120300           MOVE +1                        TO DEC-KVDECIMAL                
120400           CALL WDECEDIT USING DEC-WDECAREA                               
120500           IF DEC-KDSVAR-FEL                                              
120600             IF FEL-EJ-FUNNET                                             
120700               PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                       
120800               MOVE FEL6(INDX)        TO MOD-TEMFSFEL                     
120900               MOVE JA                TO WS-FEL-FUNNET                    
121000             END-IF                                                       
121100             MOVE MFS-NUM-FAELT-FEL TO                                    
121200                     MOD-VKORDBTO-ING-NY-ATTR                             
121300           ELSE                                                           
121400             IF DEC-IDEDITDATA = ZERO                                     
121500               IF FEL-EJ-FUNNET                                           
121600                PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                      
121700                MOVE FEL5(INDX)    TO MOD-TEMFSFEL                        
121800                MOVE JA            TO WS-FEL-FUNNET                       
121900               END-IF                                                     
122000               MOVE MFS-NUM-FAELT-FEL TO                                  
122100                         MOD-VKORDBTO-ING-NY-ATTR                         
122200             ELSE                                                         
122300               MOVE DEC-IDEDITDATA    TO WS-VKORDBTO-ING-IN               
122400             END-IF                                                       
122500           END-IF                                                         
122600         END-IF                                                           
122700                                                                          
122800       END-IF                                                             
122900     END-IF                                                               
123000     .                                                                    
123100     EJECT                                                                
123200 BC-KONTROLLERA-NYTT-ING-KOLLI SECTION.                                   
123300                                                                          
123400     IF  MID-IDKOLLI-ING-NY  = ALL '+'                                    
123500     OR  MID-VKORDBTO-ING-NY = ALL '+'                                    
123600     OR  MID-KDKOLLI-ING-NY  = ALL '+'                                    
123700                                                                          
123800       IF FEL-EJ-FUNNET                                                   
123900        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
124000        MOVE FEL6(INDX)          TO MOD-TEMFSFEL                          
124100        MOVE JA                  TO WS-FEL-FUNNET                         
124200       END-IF                                                             
124300       MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ING-NY-ATTR                  
124400                                 MOD-VKORDBTO-ING-NY-ATTR                 
124500                                 MOD-KDKOLLI-ING-NY-ATTR                  
124600     ELSE                                                                 
124700                                                                          
124800       MOVE JA                 TO WS-TILLAEGG-NYTT-ING-KOLLI              
124900                                                                          
125000       INSPECT MID-IDKOLLI-ING-NY                                         
125100                              REPLACING LEADING SPACE BY ZERO             
125200       IF MID-IDKOLLI-ING-NY  NUMERIC                                     
125300        MOVE WS-IDPRODNR        TO W-IDPRODNR                             
125400        MOVE MID-IDKOLLI-ING-NY TO W-IDKOLLI                              
125500                                                                          
125600        PERFORM IMS-GNP-FIRST-E611-KVAL                                   
125700                                                                          
125800        EVALUATE TRUE                                                     
125900        WHEN SEGMENT-SAKNAS                                               
126000          IF FEL-EJ-FUNNET                                                
126100            PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                          
126200            MOVE FEL7(INDX)      TO MOD-TEMFSFEL                          
126300            MOVE JA              TO WS-FEL-FUNNET                         
126400          END-IF                                                          
126500          MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ING-NY-ATTR               
126600        WHEN KOLLI-KDKOLSTA = ZERO AND KOLLI-FLBANDST = JA                
126700          MOVE KOLLI-IDKOLLI      TO WS-IDKOLLI-ING-NY                    
126800          MOVE KOLLI-SUORDV-KOLLI TO WS-SUORDV-KOLLI-NY                   
126900          MOVE KOLLI-SUORDV-LOC   TO WS-SUORDV-KOLLI-NY-LOC               
127000          MOVE KOLLI-SUORDV-LOCPREL                                       
127100                                  TO WS-SUORDV-KOLLI-NY-LOCPREL           
127200        WHEN OTHER                                                        
127300          IF FEL-EJ-FUNNET                                                
127400            PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                          
127500            MOVE FEL7(INDX)      TO MOD-TEMFSFEL                          
127600            MOVE JA              TO WS-FEL-FUNNET                         
127700          END-IF                                                          
127800          MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ING-NY-ATTR               
127900        END-EVALUATE                                                      
128000       ELSE                                                               
128100        IF FEL-EJ-FUNNET                                                  
128200         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
128300         MOVE FEL6(INDX)        TO MOD-TEMFSFEL                           
128400         MOVE JA                TO WS-FEL-FUNNET                          
128500        END-IF                                                            
128600        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-ING-NY-ATTR                 
128700       END-IF                                                             
128800                                                                          
128900      MOVE MID-VKORDBTO-ING-NY         TO DEC-IDFRIDATA                   
129000      MOVE +6                          TO DEC-KVHELTAL                    
129100      MOVE +1                          TO DEC-KVDECIMAL                   
129200                                                                          
129300      CALL WDECEDIT USING DEC-WDECAREA                                    
129400                                                                          
129500      IF DEC-KDSVAR-FEL                                                   
129600       IF FEL-EJ-FUNNET                                                   
129700        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
129800        MOVE FEL6(INDX)                TO MOD-TEMFSFEL                    
129900        MOVE JA                        TO WS-FEL-FUNNET                   
130000       END-IF                                                             
130100       MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ING-NY-ATTR                 
130200      ELSE                                                                
130300       IF DEC-IDEDITDATA = ZERO                                           
130400        IF FEL-EJ-FUNNET                                                  
130500         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
130600         MOVE FEL5(INDX)               TO MOD-TEMFSFEL                    
130700         MOVE JA                       TO WS-FEL-FUNNET                   
130800        END-IF                                                            
130900        MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ING-NY-ATTR                
131000       ELSE                                                               
131100        MOVE DEC-IDEDITDATA    TO WS-VKORDBTO-ING-NY                      
131200       END-IF                                                             
131300      END-IF                                                              
131400                                                                          
131500      IF MID-KDKOLLI-ING-NY = ALL '+' OR SPACE                            
131600       IF FEL-EJ-FUNNET                                                   
131700        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
131800        MOVE FEL6(INDX)            TO MOD-TEMFSFEL                        
131900        MOVE JA                    TO WS-FEL-FUNNET                       
132000       END-IF                                                             
132100       MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDKOLLI-ING-NY-ATTR             
132200                                                                          
132300      ELSE                                                                
132400                                                                          
132500       MOVE MID-KDKOLLI-ING-NY      TO W-K501-KDKOLLI                     
132600                                                                          
132700       PERFORM IMS-GU-K501-KVAL                                           
132800                                                                          
132900       IF SEGMENT-SAKNAS                                                  
133000        IF FEL-EJ-FUNNET                                                  
133100         PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                             
133200         MOVE FEL4(INDX)           TO MOD-TEMFSFEL                        
133300         MOVE JA                   TO WS-FEL-FUNNET                       
133400        END-IF                                                            
133500        MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDKOLLI-ING-NY-ATTR             
133600                                                                          
133700       ELSE                                                               
133800        MOVE EMB-KDKOLLI           TO WS-KDKOLLI-ING-NY                   
133900        MOVE EMB-EMBPROF           TO WS-EMBPROF-ING-NY                   
134000        MOVE EMB-KVLOCK            TO WS-KVLOCK-ING-NY                    
134100                                                                          
134200        IF MID-KDEMBTYP-ING-NY = ALL '+'                                  
134300         IF EMB-KDEMBTYP = ZERO                                           
134400          IF FEL-EJ-FUNNET                                                
134500           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
134600           MOVE FEL12(INDX)         TO MOD-TEMFSFEL                       
134700           MOVE JA                 TO WS-FEL-FUNNET                       
134800          END-IF                                                          
134900          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDEMBTYP-ING-NY-ATTR            
135000         ELSE                                                             
135100          MOVE EMB-KDEMBTYP      TO WS-KDEMBTYP-ING-NY                    
135200         END-IF                                                           
135300        ELSE                                                              
135400         INSPECT MID-KDEMBTYP-ING-NY                                      
135500                 REPLACING LEADING SPACE BY ZERO                          
135600         IF MID-KDEMBTYP-ING-NY NUMERIC                                   
135700          IF MID-KDEMBTYP-ING-NY = ZERO                                   
135800           IF EMB-KDEMBTYP = ZERO                                         
135900            IF FEL-EJ-FUNNET                                              
136000             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
136100             MOVE FEL5(INDX)       TO MOD-TEMFSFEL                        
136200             MOVE JA               TO WS-FEL-FUNNET                       
136300            END-IF                                                        
136400            MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-ING-NY-ATTR            
136500           ELSE                                                           
136600            MOVE EMB-KDEMBTYP   TO WS-KDEMBTYP-ING-NY                     
136700           END-IF                                                         
136800          ELSE                                                            
136900           MOVE MID-KDEMBTYP-ING-NY TO WS-KDEMBTYP-ING-NY                 
137000          END-IF                                                          
137100         ELSE                                                             
137200          IF FEL-EJ-FUNNET                                                
137300           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
137400           MOVE FEL6(INDX)         TO MOD-TEMFSFEL                        
137500           MOVE JA                 TO WS-FEL-FUNNET                       
137600          END-IF                                                          
137700          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDEMBTYP-ING-NY-ATTR            
137800         END-IF                                                           
137900        END-IF                                                            
138000                                                                          
138100        IF MID-DIKOLLIL-ING-NY = ALL '+'                                  
138200         IF EMB-DIKOLLIL = ZERO                                           
138300          IF FEL-EJ-FUNNET                                                
138400           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
138500           MOVE FEL12(INDX)         TO MOD-TEMFSFEL                       
138600           MOVE JA                 TO WS-FEL-FUNNET                       
138700          END-IF                                                          
138800          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIL-ING-NY-ATTR            
138900         ELSE                                                             
139000          MOVE EMB-DIKOLLIL     TO WS-DIKOLLIL-ING-NY                     
139100         END-IF                                                           
139200        ELSE                                                              
139300         INSPECT MID-DIKOLLIL-ING-NY                                      
139400                 REPLACING LEADING SPACE BY ZERO                          
139500         IF MID-DIKOLLIL-ING-NY NUMERIC                                   
139600          IF MID-DIKOLLIL-ING-NY = ZERO                                   
139700           IF EMB-DIKOLLIL = ZERO                                         
139800            IF FEL-EJ-FUNNET                                              
139900             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
140000             MOVE FEL5(INDX)       TO MOD-TEMFSFEL                        
140100             MOVE JA               TO WS-FEL-FUNNET                       
140200            END-IF                                                        
140300            MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-ING-NY-ATTR            
140400           ELSE                                                           
140500            MOVE EMB-DIKOLLIL   TO WS-DIKOLLIL-ING-NY                     
140600           END-IF                                                         
140700          ELSE                                                            
140800           MOVE MID-DIKOLLIL-ING-NY TO WS-DIKOLLIL-ING-NY                 
140900          END-IF                                                          
141000         ELSE                                                             
141100          IF FEL-EJ-FUNNET                                                
141200           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
141300           MOVE FEL6(INDX)         TO MOD-TEMFSFEL                        
141400           MOVE JA                 TO WS-FEL-FUNNET                       
141500          END-IF                                                          
141600          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIL-ING-NY-ATTR            
141700         END-IF                                                           
141800        END-IF                                                            
141900                                                                          
142000        IF MID-DIKOLLIB-ING-NY = ALL '+'                                  
142100         IF EMB-DIKOLLIB = ZERO                                           
142200          IF FEL-EJ-FUNNET                                                
142300           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
142400           MOVE FEL12(INDX)        TO MOD-TEMFSFEL                        
142500           MOVE JA                 TO WS-FEL-FUNNET                       
142600          END-IF                                                          
142700          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIB-ING-NY-ATTR            
142800         ELSE                                                             
142900          MOVE EMB-DIKOLLIB     TO WS-DIKOLLIB-ING-NY                     
143000         END-IF                                                           
143100        ELSE                                                              
143200         INSPECT MID-DIKOLLIB-ING-NY                                      
143300                 REPLACING LEADING SPACE BY ZERO                          
143400         IF MID-DIKOLLIB-ING-NY NUMERIC                                   
143500          IF MID-DIKOLLIB-ING-NY = ZERO                                   
143600           IF EMB-DIKOLLIB = ZERO                                         
143700            IF FEL-EJ-FUNNET                                              
143800             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
143900             MOVE FEL5(INDX)       TO MOD-TEMFSFEL                        
144000             MOVE JA               TO WS-FEL-FUNNET                       
144100            END-IF                                                        
144200            MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-ING-NY-ATTR            
144300           ELSE                                                           
144400            MOVE EMB-DIKOLLIB   TO WS-DIKOLLIB-ING-NY                     
144500           END-IF                                                         
144600          ELSE                                                            
144700           MOVE MID-DIKOLLIB-ING-NY TO WS-DIKOLLIB-ING-NY                 
144800          END-IF                                                          
144900         ELSE                                                             
145000          IF FEL-EJ-FUNNET                                                
145100           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
145200           MOVE FEL6(INDX)         TO MOD-TEMFSFEL                        
145300           MOVE JA                 TO WS-FEL-FUNNET                       
145400          END-IF                                                          
145500          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIB-ING-NY-ATTR            
145600         END-IF                                                           
145700        END-IF                                                            
145800                                                                          
145900        IF MID-DIKOLLIH-ING-NY = ALL '+'                                  
146000         IF EMB-DIKOLLIH = ZERO                                           
146100          IF FEL-EJ-FUNNET                                                
146200           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
146300           MOVE FEL12(INDX)        TO MOD-TEMFSFEL                        
146400           MOVE JA                 TO WS-FEL-FUNNET                       
146500          END-IF                                                          
146600          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIH-ING-NY-ATTR            
146700         ELSE                                                             
146800          MOVE EMB-DIKOLLIH     TO WS-DIKOLLIH-ING-NY                     
146900         END-IF                                                           
147000        ELSE                                                              
147100         INSPECT MID-DIKOLLIH-ING-NY                                      
147200                 REPLACING LEADING SPACE BY ZERO                          
147300         IF MID-DIKOLLIH-ING-NY NUMERIC                                   
147400          IF MID-DIKOLLIH-ING-NY = ZERO                                   
147500           IF EMB-DIKOLLIH = ZERO                                         
147600            IF FEL-EJ-FUNNET                                              
147700             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
147800             MOVE FEL5(INDX)       TO MOD-TEMFSFEL                        
147900             MOVE JA               TO WS-FEL-FUNNET                       
148000            END-IF                                                        
148100            MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-ING-NY-ATTR            
148200           ELSE                                                           
148300            MOVE EMB-DIKOLLIH   TO WS-DIKOLLIH-ING-NY                     
148400           END-IF                                                         
148500          ELSE                                                            
148600           MOVE MID-DIKOLLIH-ING-NY TO WS-DIKOLLIH-ING-NY                 
148700          END-IF                                                          
148800         ELSE                                                             
148900          IF FEL-EJ-FUNNET                                                
149000           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
149100           MOVE FEL6(INDX)         TO MOD-TEMFSFEL                        
149200           MOVE JA                 TO WS-FEL-FUNNET                       
149300          END-IF                                                          
149400          MOVE MFS-NUM-FAELT-FEL   TO MOD-DIKOLLIH-ING-NY-ATTR            
149500         END-IF                                                           
149600        END-IF                                                            
149700                                                                          
149800        IF FEL-EJ-FUNNET                                                  
149900          COMPUTE WS-VLORDBTO-ING-NY =                                    
150000          WS-DIKOLLIL-ING-NY * WS-DIKOLLIB-ING-NY                         
150100          * WS-DIKOLLIH-ING-NY / 1000000                                  
150200        END-IF                                                            
150300                                                                          
150400       END-IF                                                             
150500      END-IF                                                              
150600     END-IF                                                               
150700     .                                                                    
150800     EJECT                                                                
150900 C-UPPDATERING  SECTION.                                                  
151000                                                                          
151100     IF BAERKOLLI-AENDRAS                                                 
151200       PERFORM CA-UPPDATERA-BAERKOLLI                                     
151300     END-IF                                                               
151400     IF FEL-EJ-FUNNET                                                     
151500       IF ING-KOLLI-AENDRAS                                               
151600         PERFORM CB-UPPDATERA-ING-KOLLI                                   
151700       END-IF                                                             
151800       IF TILLAEGG-NYTT-ING-KOLLI                                         
151900         PERFORM CC-TILLAEGG-NYTT-ING-KOLLI                               
152000       END-IF                                                             
152100     END-IF                                                               
152200     .                                                                    
152300     EJECT                                                                
152400 CA-UPPDATERA-BAERKOLLI SECTION.                                          
152500                                                                          
152600     MOVE WS-IDPRODNR TO W-IDPRODNR                                       
152700     COMPUTE W-IDKOLLI-FLER = -1 * WS-IDKOLLI-FLER                        
152800     PERFORM IMS-GHNP-FIRST-FLER-E611-KVAL                                
152900     EVALUATE TRUE                                                        
153000     WHEN SEGMENT-FINNS AND                                               
153100         (KOLLI-ADFLOMR < +500 OR > +899)                                 
153200       PERFORM UNTIL SEGMENT-SAKNAS OR KOLLI-IDKOLLI < ZERO               
153300         PERFORM IMS-GHNP-NEXT-FLER-E611-KVAL                             
153400       END-PERFORM                                                        
153500       IF SEGMENT-FINNS AND KOLLI-IDKOLLI < ZERO                          
153600         IF WS-KDKOLLI-BAER-IN NOT = SPACE                                
153700           MOVE WS-KDKOLLI-BAER-IN TO KOLLI-KDKOLLI                       
153800         END-IF                                                           
153900         IF WS-KDEMBTYP-BAER-IN NOT = ZERO                                
154000           MOVE WS-KDEMBTYP-BAER-IN TO KOLLI-KDEMBTYP                     
154100         END-IF                                                           
154200         IF WS-DIKOLLIL-BAER-IN NOT = ZERO                                
154300           MOVE WS-DIKOLLIL-BAER-IN TO KOLLI-DIKOLLIL                     
154400         END-IF                                                           
154500         IF WS-DIKOLLIB-BAER-IN NOT = ZERO                                
154600           MOVE WS-DIKOLLIB-BAER-IN TO KOLLI-DIKOLLIB                     
154700         END-IF                                                           
154800         IF WS-DIKOLLIH-BAER-IN NOT = ZERO                                
154900           MOVE WS-DIKOLLIH-BAER-IN TO KOLLI-DIKOLLIH                     
155000         END-IF                                                           
155100         PERFORM IMS-REPLACE-E611                                         
155200       END-IF                                                             
155300     WHEN SEGMENT-FINNS                                                   
155400       PERFORM CAA-INPUT-TILL-PLATSSOEKNING                               
155500       IF FEL-EJ-FUNNET                                                   
155600         PERFORM CAB-INPUT-TILL-PLATSAVBOKNING                            
155700         IF FEL-EJ-FUNNET                                                 
155800           PERFORM UNTIL SEGMENT-SAKNAS                                   
155900                                                                          
156000             IF KOLLI-IDKOLLI < ZERO                                      
156100               MOVE SPAR-DIKOLLIH TO KOLLI-DIKOLLIH                       
156200               MOVE SPAR-DIKOLLIB TO KOLLI-DIKOLLIB                       
156300               MOVE SPAR-DIKOLLIL TO KOLLI-DIKOLLIL                       
156400               IF WS-KDKOLLI-BAER-IN NOT = SPACE                          
156500                 MOVE WS-KDKOLLI-BAER-IN TO KOLLI-KDKOLLI                 
156600               END-IF                                                     
156700               IF WS-KDEMBTYP-BAER-IN NOT = ZERO                          
156800                 MOVE WS-KDEMBTYP-BAER-IN TO KOLLI-KDEMBTYP               
156900               END-IF                                                     
157000             END-IF                                                       
157100                                                                          
157200             MOVE SPAR-ADFLGEO    TO KOLLI-ADFLGEO                        
157300             MOVE SPAR-ADFLOMR    TO KOLLI-ADFLOMR                        
157400             MOVE SPAR-ADRUTNIV   TO KOLLI-ADRUTNIV                       
157500             MOVE SPAR-DIHMODUL   TO KOLLI-DIHMODUL                       
157600             MOVE SPAR-DIDMODUL   TO KOLLI-DIDMODUL                       
157700             MOVE SPAR-ADVMODUL   TO KOLLI-ADVMODUL                       
157800             MOVE SPAR-ADHMODUL   TO KOLLI-ADHMODUL                       
157900             MOVE SPAR-IDTRPTNR   TO KOLLI-IDTRPTNR                       
158000             MOVE SPAR-FLUTLAST   TO KOLLI-FLUTLAST                       
158200                                                                          
158300             PERFORM IMS-REPLACE-E611                                     
158400             PERFORM IMS-GHNP-NEXT-FLER-E611-KVAL                         
158500           END-PERFORM                                                    
158600         END-IF                                                           
158700       END-IF                                                             
158800     END-EVALUATE                                                         
158900     .                                                                    
159000     EJECT                                                                
159100 CAA-INPUT-TILL-PLATSSOEKNING SECTION.                                    
159200                                                                          
159300                                                                          
159400     MOVE SPACE                 TO PLATS-ADFLGEO                          
159500                                   PLATS-FLUTLAST                         
159600                                   PLATS-IDDC-CROSS                       
159700     MOVE ZERO                  TO PLATS-KDCALL                           
159800                                   PLATS-IDTRPTNR                         
159900                                   PLATS-ADFLOMR                          
160000                                   PLATS-ADRUTNIV                         
160100                                   PLATS-ADVMODUL                         
160200                                   PLATS-ADHMODUL                         
160300                                   PLATS-DIHMODUL                         
160400                                   PLATS-DIDMODUL                         
160500     MOVE WS-IDDC               TO PLATS-IDDC                             
160600     MOVE WS-IDDISTR            TO PLATS-IDDISTR                          
160700     MOVE WS-IDKUNDNR           TO PLATS-IDKUNDNR                         
160800     MOVE VORD-KDFRAKT          TO PLATS-KDFRAKT                          
160900     MOVE VORD-KDORDKL          TO PLATS-KDORDKLX                         
161000     MOVE WS-IDORDNR            TO PLATS-IDORDNR                          
161100     IF WS-DIKOLLIL-BAER-IN = ZERO                                        
161200       MOVE KOLLI-DIKOLLIL      TO PLATS-DIKOLLIL                         
161300     ELSE                                                                 
161400       MOVE WS-DIKOLLIL-BAER-IN TO PLATS-DIKOLLIL                         
161500     END-IF                                                               
161600     IF WS-DIKOLLIB-BAER-IN = ZERO                                        
161700       MOVE KOLLI-DIKOLLIB      TO PLATS-DIKOLLIB                         
161800     ELSE                                                                 
161900       MOVE WS-DIKOLLIB-BAER-IN TO PLATS-DIKOLLIB                         
162000     END-IF                                                               
162100     IF WS-DIKOLLIH-BAER-IN = ZERO                                        
162200       MOVE KOLLI-DIKOLLIH      TO PLATS-DIKOLLIH                         
162300     ELSE                                                                 
162400       MOVE WS-DIKOLLIH-BAER-IN TO PLATS-DIKOLLIH                         
162500     END-IF                                                               
162600     IF WS-KDKOLLID-BAER = SPACE                                          
162700       MOVE 'L'                 TO PLATS-KDKOLLID                         
162800     ELSE                                                                 
162900       MOVE WS-KDKOLLID-BAER    TO PLATS-KDKOLLID                         
163000     END-IF                                                               
163100     MOVE +1001                 TO PLATS-VKORDNTO-KOLLI                   
163200                                                                          
163300     CALL W403PLAT USING PLATS-W403PLAT                                   
163400                         PLATS-DM-PCB                                     
163500                         PLATS-DN-PCB                                     
163600                         PLATS-DP-PCB                                     
163700                         PLATS-DO-PCB                                     
163800                         PLATS-WDE6C-PCB                                  
163900                         PLATS-GMTC-PCB                                   
164000                         PLATS-WDB6-PCB                                   
164100                                                                          
164200     IF PLATS-KDSVAR = FEL                                                
164300         IF FEL-EJ-FUNNET                                                 
164400             MOVE FEL10(INDX)   TO MOD-TEMFSFEL                           
164500             MOVE JA            TO WS-FEL-FUNNET                          
164600             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
164700         END-IF                                                           
164800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-BAER-IN-ATTR              
164900         MOVE MFS-NUM-FAELT-FEL  TO MOD-KDEMBTYP-BAER-IN-ATTR             
165000                                    MOD-DIKOLLIL-BAER-IN-ATTR             
165100                                    MOD-DIKOLLIB-BAER-IN-ATTR             
165200                                    MOD-DIKOLLIH-BAER-IN-ATTR             
165300     ELSE                                                                 
165400         MOVE JA TO WS-PLATSSOEKNING-UTFOERD                              
165500         MOVE PLATS-IDTRPTNR        TO SPAR-IDTRPTNR                      
165600         MOVE PLATS-ADCLGEO         TO SPAR-ADCLGEO                       
165700         MOVE PLATS-ADFLOMR         TO SPAR-ADFLOMR                       
165800         MOVE PLATS-ADRUTNIV        TO SPAR-ADRUTNIV                      
165900         MOVE PLATS-DIHMODUL        TO SPAR-DIHMODUL                      
166000         MOVE PLATS-DIDMODUL        TO SPAR-DIDMODUL                      
166100         MOVE PLATS-ADVMODUL        TO SPAR-ADVMODUL                      
166200         MOVE PLATS-ADHMODUL        TO SPAR-ADHMODUL                      
166300         MOVE PLATS-DIKOLLIH        TO SPAR-DIKOLLIH                      
166400         MOVE PLATS-DIKOLLIB        TO SPAR-DIKOLLIB                      
166500         MOVE PLATS-DIKOLLIL        TO SPAR-DIKOLLIL                      
166600         MOVE PLATS-FLUTLAST        TO SPAR-FLUTLAST                      
166700         MOVE PLATS-IDDC-CROSS      TO SPAR-IDDC-CROSS                    
166800                                                                          
166900         PERFORM S03-NOLLA-PLATSAREAN                                     
167000     END-IF                                                               
167100     .                                                                    
167200     EJECT                                                                
167300 CAB-INPUT-TILL-PLATSAVBOKNING SECTION.                                   
167400                                                                          
167500                                                                          
167600     MOVE +3                    TO PLATS-KDCALL                           
167700     MOVE MID-IDTRPTNR          TO PLATS-IDTRPTNR                         
167800     MOVE WS-IDDC               TO PLATS-IDDC                             
167900     MOVE MID-ADFLGEO           TO PLATS-ADFLGEO                          
168000     MOVE MID-ADFLOMR           TO PLATS-ADFLOMR                          
168100     MOVE MID-ADRUTNIV          TO PLATS-ADRUTNIV                         
168200     MOVE MID-DIHMODUL          TO PLATS-DIHMODUL                         
168300     MOVE MID-DIDMODUL          TO PLATS-DIDMODUL                         
168400     MOVE MID-ADVMODUL          TO PLATS-ADVMODUL                         
168500     MOVE MID-ADHMODUL          TO PLATS-ADHMODUL                         
168600                                                                          
168700     CALL W403PLAT USING PLATS-W403PLAT                                   
168800                         PLATS-DM-PCB                                     
168900                         PLATS-DN-PCB                                     
169000                         PLATS-DP-PCB                                     
169100                         PLATS-DO-PCB                                     
169200                         PLATS-WDE6C-PCB                                  
169300                         PLATS-GMTC-PCB                                   
169400                         PLATS-WDB6-PCB                                   
169500                                                                          
169600                                                                          
169700     IF PLATS-KDSVAR = FEL                                                
169800         IF FEL-EJ-FUNNET                                                 
169900             MOVE FEL13(INDX)   TO MOD-TEMFSFEL                           
170000             MOVE JA            TO WS-FEL-FUNNET                          
170100             PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                         
170200         END-IF                                                           
170300         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-BAER-IN-ATTR              
170400         MOVE MFS-NUM-FAELT-FEL  TO MOD-KDEMBTYP-BAER-IN-ATTR             
170500                                    MOD-DIKOLLIL-BAER-IN-ATTR             
170600                                    MOD-DIKOLLIB-BAER-IN-ATTR             
170700                                    MOD-DIKOLLIH-BAER-IN-ATTR             
170800     END-IF                                                               
170900     .                                                                    
171000     EJECT                                                                
171100 CB-UPPDATERA-ING-KOLLI SECTION.                                          
171200                                                                          
171300     MOVE WS-IDPRODNR TO W-IDPRODNR                                       
171400     MOVE WS-IDKOLLI-ING-IN TO W-IDKOLLI                                  
171500     PERFORM IMS-GHNP-E611-KVAL                                           
171600     IF SEGMENT-FINNS                                                     
171700       IF WS-VKORDBTO-ING-IN NOT = ZERO                                   
171800         MOVE WS-VKORDBTO-ING-IN TO KOLLI-VKORDBTO-KOLLI                  
171900         IF KOLLI-VKORDBTO-KOLLI < KOLLI-VKORDNTO-KOLLI                   
172000           MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI              
172100         END-IF                                                           
172200       END-IF                                                             
172300       IF WS-KDKOLLI-ING-IN NOT = SPACE                                   
172400         MOVE WS-KDKOLLI-ING-IN TO KOLLI-KDKOLLI                          
172500       END-IF                                                             
172600       IF WS-KDEMBTYP-ING-IN NOT = ZERO                                   
172700         MOVE WS-KDEMBTYP-ING-IN TO KOLLI-KDEMBTYP                        
172800       END-IF                                                             
172900       IF WS-DIKOLLIL-ING-IN NOT = ZERO                                   
173000         MOVE WS-DIKOLLIL-ING-IN TO KOLLI-DIKOLLIL                        
173100       END-IF                                                             
173200       IF WS-DIKOLLIB-ING-IN NOT = ZERO                                   
173300         MOVE WS-DIKOLLIB-ING-IN TO KOLLI-DIKOLLIB                        
173400       END-IF                                                             
173500       IF WS-DIKOLLIH-ING-IN NOT = ZERO                                   
173600         MOVE WS-DIKOLLIH-ING-IN TO KOLLI-DIKOLLIH                        
173700       END-IF                                                             
173800       PERFORM IMS-REPLACE-E611                                           
173900     END-IF                                                               
174000     .                                                                    
174100     EJECT                                                                
174200 CC-TILLAEGG-NYTT-ING-KOLLI SECTION.                                      
174300                                                                          
174400     PERFORM CCA-REDIGERA-E601                                            
174500     PERFORM IMS-REPL-WDE601                                              
174600                                                                          
174700     MOVE WS-IDKOLLI-ING-NY TO W-IDKOLLI                                  
174800     PERFORM IMS-GHNP-E611-KVAL                                           
174900     PERFORM CCB-REDIGERA-E611                                            
175000     PERFORM IMS-REPLACE-E611                                             
175100     PERFORM CCD-UPPDAT-KDORDSTA                                          
175200     .                                                                    
175300     EJECT                                                                
175400 CCA-REDIGERA-E601 SECTION.                                               
175500                                                                          
175600     ADD +1 TO VORD-KVKOLLI                                               
175700     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
175800       IF VORD-KVKOLLI = VORD-KVKOLPAC                                    
175900       AND VORD-KDORDSTA NOT > 2                                          
176000         PERFORM CCAA-KTRL-VORD-FARDIGPACKAD                              
176100         IF  WS-VORD-FARDIGPACKAD = JA                                    
176200           MOVE 3         TO VORD-KDORDSTA                                
176300         END-IF                                                           
176400       END-IF                                                             
176500     END-IF                                                               
176600     ACCEPT VORD-TIPACKN-SK FROM DATE                                     
176700     ADD WS-VKORDBTO-ING-NY      TO VORD-VKORDBTO                         
176800     ADD WS-VLORDBTO-ING-NY      TO VORD-VLORDBTO                         
176900     ADD WS-SUORDV-KOLLI-NY      TO VORD-SUORDV-PACK                      
177000     ADD WS-SUORDV-KOLLI-NY-LOC     TO VORD-SUORDV-PACK-LOC               
177100     ADD WS-SUORDV-KOLLI-NY-LOCPREL TO VORD-SUORDV-PACK-LOCPREL           
177200     .                                                                    
177300 CCAA-KTRL-VORD-FARDIGPACKAD SECTION.                                     
177400     SKIP2                                                                
177500     MOVE JA                 TO WS-VORD-FARDIGPACKAD                      
177600                                                                          
177700     PERFORM IMS-GU-WDE401-ESEQ                                           
177800     PERFORM UNTIL ((NOT SEGMENT-FINNS)                                   
177900             OR     WS-VORD-FARDIGPACKAD = NEJ)                           
178000                                                                          
178100       IF  KORD-KVORDRAD-PACK <                                           
178200          (KORD-KVORDRAD + KORD-KVORDRAD-LEVPL)                           
178300                                                                          
178400         MOVE NEJ            TO WS-VORD-FARDIGPACKAD                      
178500       ELSE                                                               
178600         PERFORM IMS-GN-WDE401-ESEQ                                       
178700       END-IF                                                             
178800     END-PERFORM                                                          
178900                                                                          
179000     IF  WS-VORD-FARDIGPACKAD = JA                                        
179100                                                                          
179200       MOVE VORD-IDDC        TO W-Q301-MIN-IDDC                           
179300                                W-Q301-MAX-IDDC                           
179400                                WS-IDDC                                   
179500       MOVE VORD-IDPRODNR    TO W-Q301-MIN-IDPRODNR                       
179600                                W-Q301-MAX-IDPRODNR                       
179700       MOVE KORD-IDORDER     TO W-Q301-MIN-IDORDER                        
179800                                W-Q301-MAX-IDORDER                        
179900                                                                          
180000       PERFORM IMS-GU-WDQ301                                              
180100                                                                          
180200       PERFORM UNTIL ((NOT SEGMENT-FINNS)                                 
180300               OR   WS-VORD-FARDIGPACKAD = NEJ)                           
180400                                                                          
180500         IF  ODEL-KDODELSTA = 'R'                                         
180600           MOVE NEJ        TO WS-VORD-FARDIGPACKAD                        
180700         ELSE                                                             
180800           PERFORM IMS-GN-WDQ301                                          
180900         END-IF                                                           
181000       END-PERFORM                                                        
181100     END-IF                                                               
181200     .                                                                    
181300     EJECT                                                                
181400 CCB-REDIGERA-E611 SECTION.                                               
181500                                                                          
181600                                                                          
181700     COMPUTE KOLLI-IDKOLLI-FLER = WS-IDKOLLI-FLER * -1                    
181800     IF BAERKOLLI-AENDRAS AND PLATSSOEKNING-UTFOERD                       
181900       MOVE PLATS-IDTRPTNR        TO KOLLI-IDTRPTNR                       
182000       MOVE PLATS-ADCLGEO         TO KOLLI-ADCLGEO                        
182100       MOVE PLATS-ADFLOMR         TO KOLLI-ADFLOMR                        
182200       MOVE PLATS-ADRUTNIV        TO KOLLI-ADRUTNIV                       
182300       MOVE PLATS-ADVMODUL        TO KOLLI-ADVMODUL                       
182400       MOVE PLATS-ADHMODUL        TO KOLLI-ADHMODUL                       
182500       MOVE PLATS-DIHMODUL        TO KOLLI-DIHMODUL                       
182600       MOVE PLATS-DIDMODUL        TO KOLLI-DIDMODUL                       
182700       MOVE PLATS-FLUTLAST        TO KOLLI-FLUTLAST                       
182900     ELSE                                                                 
183000       MOVE MID-IDTRPTNR          TO KOLLI-IDTRPTNR                       
183100       MOVE WS-IDDC               TO KOLLI-IDDC                           
183200       MOVE MID-ADFLGEO           TO KOLLI-ADFLGEO                        
183300       MOVE MID-ADFLOMR           TO KOLLI-ADFLOMR                        
183400       MOVE MID-ADRUTNIV          TO KOLLI-ADRUTNIV                       
183500       MOVE MID-ADVMODUL          TO KOLLI-ADVMODUL                       
183600       MOVE MID-ADHMODUL          TO KOLLI-ADHMODUL                       
183700       MOVE MID-DIHMODUL          TO KOLLI-DIHMODUL                       
183800       MOVE MID-DIDMODUL          TO KOLLI-DIDMODUL                       
183900       MOVE MID-FLUTLAST          TO KOLLI-FLUTLAST                       
184000     END-IF                                                               
184100     MOVE 1                       TO KOLLI-KDKOLSTA                       
184200     ACCEPT KOLLI-TIPACKN FROM DATE                                       
184300     ACCEPT WS-TIPACTID-8 FROM TIME                                       
184400     MOVE WS-TIPACTID-6           TO KOLLI-TIPACTID                       
184500     MOVE WS-KDKOLLI-ING-NY       TO KOLLI-KDKOLLI                        
184600     MOVE WS-DIKOLLIL-ING-NY      TO KOLLI-DIKOLLIL                       
184700     MOVE WS-DIKOLLIB-ING-NY      TO KOLLI-DIKOLLIB                       
184800     MOVE WS-DIKOLLIH-ING-NY      TO KOLLI-DIKOLLIH                       
184900     MOVE WS-KDEMBTYP-ING-NY      TO KOLLI-KDEMBTYP                       
185000     MOVE WS-VKORDBTO-ING-NY      TO KOLLI-VKORDBTO-KOLLI                 
185100     MOVE WS-VLORDBTO-ING-NY      TO KOLLI-VLORDBTO-KOLLI                 
185200     .                                                                    
185300     EJECT                                                                
185400 CCD-UPPDAT-KDORDSTA SECTION.                                             
185500                                                                          
185600     PERFORM IMS-GU-ORQI01                                                
185700                                                                          
185800     MOVE OHUV-BEGMT-RAD1      TO WS-BEGMT-RAD1                           
185900     MOVE OHUV-BEGMT-RAD2      TO WS-BEGMT-RAD2                           
186000     MOVE OHUV-ADGMT-GATA      TO WS-ADGMT-GATA                           
186100     MOVE OHUV-ADGMT-PADR      TO WS-ADGMT-PADR                           
186200     MOVE OHUV-ADGMT-LAND      TO WS-ADGMT-LAND                           
186300                                                                          
186400     MOVE WS-IDDC              TO W-212-IDDC                              
186500     PERFORM IMS-GHNP-ORQI12                                              
186700     IF ARB-KDORDSTA = 'U '                                               
186800       MOVE 'U*'               TO ARB-KDORDSTA                            
186810       PERFORM IMS-REPL-ORQI12                                            
186900     END-IF                                                               
189200     .                                                                    
189300     EJECT                                                                
189400 D-KONTROLLERA-PRINTER SECTION.                                           
189500                                                                          
189600     IF WS-KDPRTVAL = 'U '                                                
189700     OR WS-KDPRTVAL = 'UU'                                                
189800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-IN-ATTR                  
189900     ELSE                                                                 
190000       MOVE '4'                TO WS-SYSTDEL                              
190100       MOVE 'KF'               TO WS-LISTTYP                              
190200       MOVE WS-IDDC            TO WS-DC                                   
190300       MOVE WS-KDPRTVAL        TO WS-KDPRT                                
190400                                                                          
190500       MOVE 001                TO PRT-KDCALL                              
190600       MOVE WS-IDPRTLST        TO PRT-IDPRTLST                            
190700                                                                          
190800       CALL W006PRT USING PRT-W006PRT                                     
190900                                                                          
191000       IF PRT-KDSVAR = FEL                                                
191100         IF FEL-EJ-FUNNET                                                 
191200           PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                           
191300           MOVE FEL11(INDX)    TO MOD-TEMFSFEL                            
191400           MOVE JA             TO WS-FEL-FUNNET                           
191500         END-IF                                                           
191600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-IN-ATTR                  
191700       END-IF                                                             
191800     END-IF                                                               
191900     .                                                                    
192000     EJECT                                                                
192100 E-PRINTA-KOLLIFLAGGA SECTION.                                            
192200                                                                          
192300                                                                          
192400     MOVE WS-IDDISTR            TO 4333-MID-IDDISTR-UT                    
192500     MOVE WS-IDKUNDNR           TO 4333-MID-IDKUNDNR-UT                   
192600     MOVE WS-IDORDNR            TO 4333-MID-IDORDNR-UT                    
192700     MOVE WS-IDKOLLI            TO 4333-MID-IDKOLLI-UT                    
192800     MOVE WS-IDDC               TO 4333-MID-IDDC-UT                       
192900     MOVE SPACE                 TO 4333-MID-IDPRODNR-UT                   
193000     MOVE WS-KDPRTVAL           TO 4333-MID-KDPRTVAL-UT                   
193100     MOVE ZERO                  TO 4333-MID-IDKOLLI-TOM                   
193200     MOVE '++++'                TO 4333-MID-IDDISTR-IN                    
193300     MOVE '++++++'              TO 4333-MID-IDKUNDNR-IN                   
193400     MOVE '+++++'               TO 4333-MID-IDORDNR-IN                    
193500                                   4333-MID-IDKOLLI-IN                    
193600     MOVE '++'                  TO 4333-MID-IDDC-IN                       
193700     MOVE '+++++++'             TO 4333-MID-IDPRODNR-IN                   
193800     MOVE '++'                  TO 4333-MID-KDPRTVAL-IN                   
193900                                                                          
194000     COMPUTE 4333-MID-LL = LENGTH OF 4333-MID-W4I33301 + 17               
194100     MOVE WS-MFS-KDMFSFOR       TO 4333-MID-KDMFSFOR                      
194200                                                                          
194300     PERFORM IMS-INSERT-ALT-MSG                                           
194400     MOVE MED2(INDX)            TO MOD-TEMFSINF                           
194500     .                                                                    
194600     EJECT                                                                
194700 F-FRAAGA SECTION.                                                        
194800                                                                          
194900     PERFORM FA-LAES-FOR-FLERKOLLI-ID                                     
195000     IF FEL-EJ-FUNNET                                                     
195100                                                                          
195200       EVALUATE TRUE                                                      
195300       WHEN BLADDRA-BAKAAT                                                
195400         MOVE ZERO TO WS-IDKOLLI                                          
195500       WHEN BLADDRA-FRAMAAT                                               
195600         MOVE WS-IDKOLLI-NEXT TO WS-IDKOLLI                               
195700       WHEN OTHER                                                         
195800         MOVE WS-IDKOLLI-FIRST TO WS-IDKOLLI                              
195900       END-EVALUATE                                                       
196000                                                                          
196100       MOVE WS-IDKOLLI-FLER TO W-IDKOLLI                                  
196200       PERFORM IMS-GNP-F-IDKOLLIF-E611-KVAL                               
196300                                                                          
196400                                                                          
196500       IF SEGMENT-FINNS                                                   
196600                                                                          
196700           MOVE KOLLI-KDKOLLI TO MOD-KDKOLLI-BAER-UT                      
196800           MOVE KOLLI-KDEMBTYP TO MOD-KDEMBTYP-BAER-UT                    
196900           MOVE KOLLI-DIKOLLIL TO MOD-DIKOLLIL-BAER-UT                    
197000           MOVE KOLLI-DIKOLLIB TO MOD-DIKOLLIB-BAER-UT                    
197100           MOVE KOLLI-DIKOLLIH TO MOD-DIKOLLIH-BAER-UT                    
197200           MOVE KOLLI-ADFLGEO TO MOD-ADFLGEO                              
197300           MOVE KOLLI-ADFLOMR TO MOD-ADFLOMR                              
197400           MOVE KOLLI-ADRUTNIV TO MOD-ADRUTNIV                            
197500           MOVE KOLLI-ADVMODUL TO MOD-ADVMODUL                            
197600           MOVE KOLLI-ADHMODUL TO MOD-ADHMODUL                            
197700           MOVE KOLLI-DIDMODUL TO MOD-DIDMODUL                            
197800           MOVE KOLLI-DIHMODUL TO MOD-DIHMODUL                            
197900           MOVE KOLLI-IDTRPTNR TO MOD-IDTRPTNR                            
198000           MOVE KOLLI-FLUTLAST TO MOD-FLUTLAST                            
198100                                                                          
198200       END-IF                                                             
198300                                                                          
198400       MOVE WS-IDKOLLI TO W-IDKOLLI                                       
198500       MOVE WS-IDKOLLI-FLER TO W-IDKOLLI-FLER                             
198600       PERFORM IMS-GNP-F-KOLLI-FLER-E611-KVAL                             
198700*                                                                         
198800       IF FEL-EJ-FUNNET                                                   
198900          MOVE +1 TO RAD-INDX                                             
199000          PERFORM UNTIL SEGMENT-SAKNAS OR                                 
199100                  RAD-INDX > MAX-ANTAL-ING-KOLLI-PER-SIDA                 
199200                                                                          
199300          MOVE KOLLI-IDKOLLI TO WS-JFR-IDKOLLI                            
199400          IF (KOLLI-IDKOLLI < ZERO) OR                                    
199500             (WS-JFR-IDKOLLI = MID-IDKOLLI-ING-NY AND                     
199600              FEL-FUNNET)                                                 
199700             CONTINUE                                                     
199800          ELSE                                                            
199900             IF RAD-INDX = +1                                             
200000                MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-FIRST                   
200100             END-IF                                                       
200200             MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-ING-UT (RAD-INDX)          
200300             MOVE KOLLI-VKORDBTO-KOLLI TO                                 
200400                         MOD-VKORDBTO-ING-UT (RAD-INDX)                   
200500             MOVE KOLLI-KDKOLLI TO MOD-KDKOLLI-ING-UT (RAD-INDX)          
200600             MOVE KOLLI-KDEMBTYP TO MOD-KDEMBTYP-ING-UT (RAD-INDX)        
200700             MOVE KOLLI-DIKOLLIL TO MOD-DIKOLLIL-ING-UT (RAD-INDX)        
200800             MOVE KOLLI-DIKOLLIB TO MOD-DIKOLLIB-ING-UT (RAD-INDX)        
200900             MOVE KOLLI-DIKOLLIH TO MOD-DIKOLLIH-ING-UT (RAD-INDX)        
201000             ADD +1 TO RAD-INDX                                           
201100          END-IF                                                          
201200          PERFORM IMS-GNP-N-KOLLI-FLER-E611-KVAL                          
201300          END-PERFORM                                                     
201400       END-IF                                                             
201500                                                                          
201600       IF SEGMENT-FINNS                                                   
201700         MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-NEXT                           
201800         MOVE MED3(INDX) TO MOD-TEMFSINF                                  
201900       ELSE                                                               
202000         MOVE ZERO TO MOD-IDKOLLI-NEXT                                    
202100       END-IF                                                             
202200     END-IF                                                               
202300     .                                                                    
202400     EJECT                                                                
202500 FA-LAES-FOR-FLERKOLLI-ID SECTION.                                        
202600     SKIP2                                                                
202700     MOVE WS-IDPRODNR TO MOD-IDPRODNR                                     
202800                                                                          
202900     MOVE WS-IDKOLLI  TO W-IDKOLLI                                        
203000     PERFORM IMS-GHNP-E611-KVAL                                           
203100     EVALUATE TRUE                                                        
203200     WHEN SEGMENT-SAKNAS                                                  
203300       IF FEL-EJ-FUNNET                                                   
203400        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
203500        MOVE FEL7(INDX)           TO MOD-TEMFSFEL                         
203600        MOVE JA                   TO WS-FEL-FUNNET                        
203700       END-IF                                                             
203800     WHEN KOLLI-KDKOLSTA = 1                                              
203900       IF KOLLI-IDKOLLI-FLER NOT < ZERO                                   
204000         IF FEL-EJ-FUNNET                                                 
204100          PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                            
204200          MOVE FEL8(INDX)           TO MOD-TEMFSFEL                       
204300          MOVE JA                   TO WS-FEL-FUNNET                      
204400         END-IF                                                           
204500       ELSE                                                               
204600         MOVE KOLLI-IDKOLLI-FLER TO WS-IDKOLLI-FLER                       
204700                                    MOD-IDKOLLI-FLER                      
204800       END-IF                                                             
204900     WHEN KOLLI-KDKOLSTA < 1                                              
205000       IF FEL-EJ-FUNNET                                                   
205100        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
205200        MOVE FEL9(INDX)           TO MOD-TEMFSFEL                         
205300        MOVE JA                   TO WS-FEL-FUNNET                        
205400       END-IF                                                             
205500     WHEN OTHER                                                           
205600       IF FEL-EJ-FUNNET                                                   
205700        PERFORM S02-SAETT-ADD-LAES-IGEN-ATTR                              
205800        MOVE FEL15(INDX)          TO MOD-TEMFSFEL                         
205900        MOVE JA                   TO WS-FEL-FUNNET                        
206000       END-IF                                                             
206100     END-EVALUATE                                                         
206200     .                                                                    
206300     EJECT                                                                
206400 G-TEST-OM-MAN-MENAT-PF11 SECTION.                                        
206500                                                                          
206600     MOVE NEJ TO PF11-TEST                                                
206700                                                                          
206800     IF MID-KDCMD = ALL '+' OR SPACE                                      
206900       CONTINUE                                                           
207000     ELSE                                                                 
207100       MOVE JA TO PF11-TEST                                               
207200     END-IF                                                               
207300                                                                          
207400     IF MID-KDKOLLI-BAER-IN = ALL '+' AND                                 
207500       MID-KDEMBTYP-BAER-IN = ALL '+' AND                                 
207600       MID-DIKOLLIL-BAER-IN = ALL '+' AND                                 
207700       MID-DIKOLLIB-BAER-IN = ALL '+' AND                                 
207800       MID-DIKOLLIH-BAER-IN = ALL '+'                                     
207900                                                                          
208000       CONTINUE                                                           
208100     ELSE                                                                 
208200       MOVE JA TO PF11-TEST                                               
208300     END-IF                                                               
208400     .                                                                    
208500     EJECT                                                                
208600 H-RENSA-NYCKLAR SECTION.                                                 
208700     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                          
208800                                  MOD-IDKUNDNR-UT                         
208900                                  MOD-IDORDNR-UT                          
209000                                  MOD-IDKOLLI-UT                          
209100                                  MOD-KDPRTVAL-UT                         
209200     .                                                                    
209300     EJECT                                                                
209400 I-HAEMTA-STARTNYCKEL SECTION.                                            
209500     MOVE 'N'                 TO WS-SLINGA-KLAR                           
209600*                                                                         
209700     IF WS-IDDISTR NUMERIC  AND                                           
209800        WS-IDKUNDNR NUMERIC AND                                           
209900        WS-IDORDNR  NUMERIC AND                                           
210000        WS-IDKOLLI  NUMERIC                                               
210100        MOVE WS-IDDISTR       TO W-IDDISTR                                
210200        MOVE WS-IDKUNDNR      TO W-IDKUNDNR                               
210300        MOVE WS-IDORDNR       TO W-IDORDNR                                
210400        PERFORM IMS-GU-WDE401-ASEK                                        
210500*                                                                         
210600        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
210700                                     OR SLINGA-KLAR                       
210800          IF KORD-IDDC = WS-IDDC AND KORD-KVORDRAD-LEVPL = ZERO           
210900            MOVE KORD-IDPRODNR TO W-IDPRODNR                              
211000            PERFORM IMS-GHU-WDE601                                        
211100                                                                          
211200            MOVE 'J'       TO WS-SLINGA-KLAR                              
211300          ELSE                                                            
211400             PERFORM IMS-GN-WDE401-ASEK                                   
211500          END-IF                                                          
211600        END-PERFORM                                                       
211700                                                                          
211800        IF WS-SLINGA-KLAR = 'N'                                           
211900           MOVE FEL2(INDX)           TO MOD-TEMFSFEL                      
212000           MOVE JA                   TO WS-FEL-FUNNET                     
212100        END-IF                                                            
212200     ELSE                                                                 
212300        IF FEL-EJ-FUNNET                                                  
212400           MOVE FEL2(INDX)             TO MOD-TEMFSFEL                    
212500           MOVE JA                     TO WS-FEL-FUNNET                   
212600        END-IF                                                            
212700     END-IF                                                               
212800*                                                                         
212900     IF NOT SLINGA-KLAR                                                   
213000        IF FEL-EJ-FUNNET                                                  
213100           MOVE FEL2(INDX)             TO MOD-TEMFSFEL                    
213200           MOVE JA                     TO WS-FEL-FUNNET                   
213300        END-IF                                                            
213400     END-IF                                                               
213500     .                                                                    
213600     EJECT                                                                
213700 S01-NOLLSTAELL SECTION.                                                  
213800                                                                          
213900                                                                          
214000     MOVE SPACE                 TO WS-BAERKOLLI-AENDRAS                   
214100                                   WS-BAER-KDKOLLI-AENDRAS                
214200                                   WS-KDKOLLI-BAER-IN                     
214300                                   WS-KDKOLLID-BAER                       
214400     MOVE ZERO                  TO WS-KDEMBTYP-BAER-IN                    
214500                                   WS-DIKOLLIL-BAER-IN                    
214600                                   WS-DIKOLLIB-BAER-IN                    
214700                                   WS-DIKOLLIH-BAER-IN                    
214800                                                                          
214900     MOVE SPACE                 TO WS-ING-KOLLI-AENDRAS                   
215000                                   WS-ING-KDKOLLI-AENDRAS                 
215100                                   WS-KDKOLLI-ING-IN                      
215200     MOVE ZERO TO                  WS-IDKOLLI-ING-IN                      
215300                                   WS-VKORDBTO-ING-IN                     
215400                                   WS-KDEMBTYP-ING-IN                     
215500                                   WS-DIKOLLIL-ING-IN                     
215600                                   WS-DIKOLLIB-ING-IN                     
215700                                   WS-DIKOLLIH-ING-IN                     
215800                                                                          
215900     MOVE SPACE TO                 WS-TILLAEGG-NYTT-ING-KOLLI             
216000                                   WS-KDKOLLI-ING-NY                      
216100                                   WS-EMBPROF-ING-NY                      
216200     MOVE ZERO TO                  WS-IDKOLLI-ING-NY                      
216300                                   WS-VKORDBTO-ING-NY                     
216400                                   WS-KDEMBTYP-ING-NY                     
216500                                   WS-DIKOLLIL-ING-NY                     
216600                                   WS-DIKOLLIB-ING-NY                     
216700                                   WS-DIKOLLIH-ING-NY                     
216800                                   WS-KVLOCK-ING-NY                       
216900     .                                                                    
217000     EJECT                                                                
217100 S02-SAETT-ADD-LAES-IGEN-ATTR SECTION.                                    
217200                                                                          
217300                                                                          
217400     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDKOLLI-FLER                       
217500                                   MOD-IDKOLLI-FIRST                      
217600                                   MOD-IDKOLLI-NEXT                       
217700                                   MOD-IDPRODNR                           
217800                                   MOD-KDKOLLI-BAER-UT                    
217900                                   MOD-KDKOLLI-BAER-IN                    
218000                                   MOD-KDEMBTYP-BAER-UT                   
218100                                   MOD-KDEMBTYP-BAER-IN                   
218200                                   MOD-DIKOLLIL-BAER-UT                   
218300                                   MOD-DIKOLLIL-BAER-IN                   
218400                                   MOD-DIKOLLIB-BAER-UT                   
218500                                   MOD-DIKOLLIB-BAER-IN                   
218600                                   MOD-DIKOLLIH-BAER-UT                   
218700                                   MOD-DIKOLLIH-BAER-IN                   
218800                                   MOD-KDCMD                              
218900                                   MOD-IDKOLLI-ING-NY                     
219000                                   MOD-VKORDBTO-ING-NY                    
219100                                   MOD-KDKOLLI-ING-NY                     
219200                                   MOD-KDEMBTYP-ING-NY                    
219300                                   MOD-DIKOLLIL-ING-NY                    
219400                                   MOD-DIKOLLIB-ING-NY                    
219500                                   MOD-DIKOLLIH-ING-NY                    
219600                                   MOD-ADRESS-TEXT                        
219700                                   MOD-ADFLGEO                            
219800                                   MOD-ADFLOMR                            
219900                                   MOD-ADRUTNIV                           
220000                                   MOD-ADVMODUL                           
220100                                   MOD-ADHMODUL                           
220200                                   MOD-DIDMODUL                           
220300                                   MOD-DIHMODUL                           
220400                                   MOD-IDTRPTNR                           
220500                                   MOD-FLUTLAST                           
220600                                   MOD-TEMFSINF                           
220700                                                                          
220800                                                                          
220900     MOVE +1                    TO RAD-INDX                               
221000     PERFORM UNTIL RAD-INDX > MAX-ANTAL-ING-KOLLI-PER-SIDA                
221100      MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-ING-UT(RAD-INDX)              
221200                                MOD-VKORDBTO-ING-UT(RAD-INDX)             
221300                                MOD-KDKOLLI-ING-UT(RAD-INDX)              
221400                                MOD-KDEMBTYP-ING-UT(RAD-INDX)             
221500                                MOD-DIKOLLIL-ING-UT(RAD-INDX)             
221600                                MOD-DIKOLLIB-ING-UT(RAD-INDX)             
221700                                MOD-DIKOLLIH-ING-UT(RAD-INDX)             
221800      ADD +1                 TO RAD-INDX                                  
221900     END-PERFORM                                                          
222000                                                                          
222100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-IN-ATTR                   
222200                                   MOD-KDKOLLI-BAER-IN-ATTR               
222300                                   MOD-KDEMBTYP-BAER-IN-ATTR              
222400                                   MOD-DIKOLLIL-BAER-IN-ATTR              
222500                                   MOD-DIKOLLIB-BAER-IN-ATTR              
222600                                   MOD-DIKOLLIH-BAER-IN-ATTR              
222700                                   MOD-KDCMD-ATTR                         
222800                                   MOD-IDKOLLI-ING-NY-ATTR                
222900                                   MOD-VKORDBTO-ING-NY-ATTR               
223000                                   MOD-KDKOLLI-ING-NY-ATTR                
223100                                   MOD-KDEMBTYP-ING-NY-ATTR               
223200                                   MOD-DIKOLLIL-ING-NY-ATTR               
223300                                   MOD-DIKOLLIB-ING-NY-ATTR               
223400                                   MOD-DIKOLLIH-ING-NY-ATTR               
223500     .                                                                    
223600     EJECT                                                                
223700 S03-NOLLA-PLATSAREAN SECTION.                                            
223800                                                                          
223900     MOVE ZERO                  TO PLATS-KDCALL                           
224000                                   PLATS-IDDISTR                          
224100                                   PLATS-IDKUNDNR                         
224200                                   PLATS-KDFRAKT                          
224300                                   PLATS-IDORDNR                          
224400                                   PLATS-DIKOLLIH                         
224500                                   PLATS-DIKOLLIB                         
224600                                   PLATS-DIKOLLIL                         
224700                                   PLATS-VKORDNTO-KOLLI                   
224800                                   PLATS-IDTRPTNR                         
224900                                   PLATS-ADFLOMR                          
225000                                   PLATS-ADRUTNIV                         
225100                                   PLATS-DIHMODUL                         
225200                                   PLATS-DIDMODUL                         
225300                                   PLATS-ADVMODUL                         
225400                                   PLATS-ADHMODUL                         
225500                                   PLATS-VLRUTNIV                         
225600                                   PLATS-TIRFS                            
225700     MOVE SPACE                 TO PLATS-KDORDKLX                         
225800                                   PLATS-KDKOLLID                         
225900                                   PLATS-IDDC                             
226000                                   PLATS-ADFLGEO                          
226100                                   PLATS-FLUTLAST                         
226200                                   PLATS-TESPAERR                         
226300     .                                                                    
226400     EJECT                                                                
226500* IMS SEKTIONER                                                           
226600     SKIP3                                                                
226700 IMS-GET-MSG SECTION.                                                     
226800     MOVE '  QC' TO GODK-STATUSKODER                                      
226900     CALL CBLTDLI USING GU                                                
227000                          MSG-PCB                                         
227100                          MSG-IO-AREA                                     
227200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     SKIP3                                                                
227500     .                                                                    
227600 IMS-INSERT-MSG SECTION.                                                  
227700                                                                          
227800     IF NOT ENGLISH-TEXT                                                  
227900       MOVE '0' TO MFS-KDHUVOMR                                           
228000     END-IF                                                               
228100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
228200     MOVE SPACE TO GODK-STATUSKODER                                       
228300     CALL CBLTDLI USING ISRT                                              
228400                          MSG-PCB                                         
228500                          MSG-IO-AREA                                     
228600                          MFS-IDMOD                                       
228700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
228800     PERFORM IMS-STATUSKONTROLL                                           
228900     SKIP3                                                                
229000     .                                                                    
229100 IMS-INSERT-ALT-MSG SECTION.                                              
229200     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
229300     MOVE SPACE TO GODK-STATUSKODER                                       
229400     CALL CBLTDLI USING ISRT                                              
229500                          ALT-PCB                                         
229600                          4333-MID-IO-AREA                                
229700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
229800     PERFORM IMS-STATUSKONTROLL                                           
229900     .                                                                    
230000     EJECT                                                                
230100 IMS-GU-K501-KVAL SECTION.                                                
230200     STRING 'WDK501  (KDKOLLI  =' W-K501-KDKOLLI-X ')'                    
230300            DELIMITED BY SIZE INTO SSA1                                   
230400     MOVE '  GE' TO GODK-STATUSKODER                                      
230500     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-K501 SSA1                      
230600     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
230700     PERFORM IMS-STATUSKONTROLL                                           
230800     .                                                                    
230900     EJECT                                                                
231000 IMS-GU-WDE401-ASEK SECTION.                                              
231100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
231200            DELIMITED BY SIZE INTO SSA1                                   
231300     MOVE '  GE' TO GODK-STATUSKODER                                      
231400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
231500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
231600     PERFORM IMS-STATUSKONTROLL                                           
231700     SKIP3                                                                
231800     .                                                                    
231900 IMS-GN-WDE401-ASEK SECTION.                                              
232000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
232100            DELIMITED BY SIZE INTO SSA1                                   
232200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
232300     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
232400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
232500     PERFORM IMS-STATUSKONTROLL                                           
232600     SKIP3                                                                
232700     .                                                                    
232800 IMS-GHU-WDE601 SECTION.                                                  
232900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
233000            DELIMITED BY SIZE INTO SSA1                                   
233100     MOVE '  ' TO GODK-STATUSKODER                                        
233200     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
233300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
233400     PERFORM IMS-STATUSKONTROLL                                           
233500     SKIP2                                                                
233600     .                                                                    
233700 IMS-REPL-WDE601 SECTION.                                                 
233800     MOVE '  ' TO GODK-STATUSKODER                                        
233900     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
234000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300     EJECT                                                                
234400 IMS-GNP-F-IDKOLLIF-E611-KVAL SECTION.                                    
234500     STRING 'WDE611  *F(IDKOLLI  =' W-IDKOLLI-X ')'                       
234600            DELIMITED BY SIZE INTO SSA1                                   
234700     MOVE '  GE' TO GODK-STATUSKODER                                      
234800     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
234900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
235000     PERFORM IMS-STATUSKONTROLL                                           
235100     .                                                                    
235200     EJECT                                                                
235300 IMS-GHNP-FIRST-FLER-E611-KVAL SECTION.                                   
235400     STRING 'WDE611  *F(IDKOLLIF =' W-IDKOLLI-FLER-X ')'                  
235500            DELIMITED BY SIZE INTO SSA1                                   
235600     MOVE '  GE' TO GODK-STATUSKODER                                      
235700     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E611 SSA1                    
235800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
235900     PERFORM IMS-STATUSKONTROLL                                           
236000     SKIP3                                                                
236100     .                                                                    
236200 IMS-GHNP-NEXT-FLER-E611-KVAL SECTION.                                    
236300     STRING 'WDE611  (IDKOLLIF =' W-IDKOLLI-FLER-X ')'                    
236400            DELIMITED BY SIZE INTO SSA1                                   
236500     MOVE '  GE' TO GODK-STATUSKODER                                      
236600     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E611 SSA1                    
236700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
236800     PERFORM IMS-STATUSKONTROLL                                           
236900     .                                                                    
237000     EJECT                                                                
237100 IMS-GNP-F-KOLLI-FLER-E611-KVAL SECTION.                                  
237200     STRING 'WDE611  *F(IDKOLLI =>' W-IDKOLLI-X                           
237300                      '&IDKOLLIF =' W-IDKOLLI-FLER-X ')'                  
237400            DELIMITED BY SIZE INTO SSA1                                   
237500     MOVE '  GE' TO GODK-STATUSKODER                                      
237600     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
237700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
237800     PERFORM IMS-STATUSKONTROLL                                           
237900     SKIP3                                                                
238000     .                                                                    
238100 IMS-GNP-N-KOLLI-FLER-E611-KVAL SECTION.                                  
238200     STRING 'WDE611  (IDKOLLI =>' W-IDKOLLI-X                             
238300                    '&IDKOLLIF =' W-IDKOLLI-FLER-X ')'                    
238400            DELIMITED BY SIZE INTO SSA1                                   
238500     MOVE '  GE' TO GODK-STATUSKODER                                      
238600     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
238700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
238800     PERFORM IMS-STATUSKONTROLL                                           
238900     .                                                                    
239000     EJECT                                                                
239100 IMS-GHNP-E611-KVAL SECTION.                                              
239200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
239300            DELIMITED BY SIZE INTO SSA1                                   
239400     MOVE '  GE' TO GODK-STATUSKODER                                      
239500     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-E611 SSA1                    
239600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
239700     PERFORM IMS-STATUSKONTROLL                                           
239800     SKIP3                                                                
239900     .                                                                    
240000 IMS-GNP-FIRST-E611-KVAL SECTION.                                         
240100     STRING 'WDE611  *F(IDKOLLI  =' W-IDKOLLI-X ')'                       
240200            DELIMITED BY SIZE INTO SSA2                                   
240300     MOVE '  GE' TO GODK-STATUSKODER                                      
240400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
240500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
240600     PERFORM IMS-STATUSKONTROLL                                           
240700     SKIP3                                                                
240800     .                                                                    
240900 IMS-REPLACE-E611 SECTION.                                                
241000     MOVE '  ' TO GODK-STATUSKODER                                        
241100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
241200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
241300     PERFORM IMS-STATUSKONTROLL                                           
241400     .                                                                    
241500     EJECT                                                                
241600 IMS-GU-WDE401-ESEQ SECTION.                                              
241700                                                                          
241800     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
241900            DELIMITED BY SIZE INTO SSA1                                   
242000     MOVE '  ' TO GODK-STATUSKODER                                        
242100     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-E401 SSA1                     
242200     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
242300     PERFORM IMS-STATUSKONTROLL                                           
242400     .                                                                    
242500     SKIP3                                                                
242600 IMS-GN-WDE401-ESEQ SECTION.                                              
242700                                                                          
242800     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
242900            DELIMITED BY SIZE INTO SSA1                                   
243000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
243100     CALL CBLTDLI USING GN WDE4E-PCB DLI-IO-E401 SSA1                     
243200     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
243300     PERFORM IMS-STATUSKONTROLL                                           
243400     .                                                                    
243500     EJECT                                                                
243600 IMS-GU-WDQ301 SECTION.                                                   
243700                                                                          
243800     STRING 'WDQ301  (WDQ301KY >' W-Q301-KEY-MIN-X                        
243900                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
244000            DELIMITED BY SIZE INTO SSA1                                   
244100     MOVE '  GE' TO GODK-STATUSKODER                                      
244200     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-Q301 SSA1                      
244300     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
244400     PERFORM IMS-STATUSKONTROLL                                           
244500     .                                                                    
244600     SKIP3                                                                
244700 IMS-GN-WDQ301 SECTION.                                                   
244800                                                                          
244900     STRING 'WDQ301  (WDQ301KY >' W-Q301-KEY-MIN-X                        
245000                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
245100            DELIMITED BY SIZE INTO SSA1                                   
245200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
245300     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-Q301 SSA1                      
245400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
245500     PERFORM IMS-STATUSKONTROLL                                           
245600     .                                                                    
245700     EJECT                                                                
245800 IMS-GU-ORQI01    SECTION.                                                
245900     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
246000            DELIMITED BY SIZE INTO SSA1                                   
246100     MOVE '    ' TO GODK-STATUSKODER                                      
246200     CALL CBLTDLI USING GU    ORQI-PCB DLI-IO-Q201 SSA1                   
246300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
246400     PERFORM IMS-STATUSKONTROLL                                           
246500     .                                                                    
246600 IMS-GHNP-ORQI12    SECTION.                                              
246700     STRING 'WLORQI12(IDDC     =' W-WDQ212-X ')'                          
246800            DELIMITED BY SIZE INTO SSA1                                   
246900     MOVE '  GE' TO GODK-STATUSKODER                                      
247000     CALL CBLTDLI USING GHNP  ORQI-PCB DLI-IO-Q212 SSA1                   
247100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
247200     PERFORM IMS-STATUSKONTROLL                                           
247300     .                                                                    
248000 IMS-REPL-ORQI12      SECTION.                                            
248100     MOVE '  ' TO GODK-STATUSKODER                                        
248200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-Q212                         
248300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
248400     PERFORM IMS-STATUSKONTROLL                                           
248500     .                                                                    
248600     EJECT                                                                
248700 IMS-GU-WDB601    SECTION.                                                
248800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
248900          DELIMITED BY SIZE INTO SSA1                                     
249000     MOVE '  '   TO GODK-STATUSKODER                                      
249100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
249200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
249300     PERFORM IMS-STATUSKONTROLL                                           
249400     .                                                                    
249500                                                                          
249600 IMS-ROLLBACK       SECTION.                                              
249700     MOVE '  ' TO GODK-STATUSKODER                                        
249800     CALL CBLTDLI USING ROLB MSG-PCB                                      
249900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
250000     PERFORM IMS-STATUSKONTROLL                                           
250100     .                                                                    
250200     EJECT                                                                
250300 IMS-STATUSKONTROLL SECTION.                                              
250400     SET STATUS-IX TO 1                                                   
250500     SEARCH GODK-STATUS AT END CALL FELLOG                                
250600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
250700     END-SEARCH                                                           
250800     SKIP2                                                                
250900     .                                                                    
251000     EJECT                                                                
