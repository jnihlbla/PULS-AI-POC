000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4033900.                                                
000300 AUTHOR.         CAMELIA OLGRENER.                                        
000400 DATE-WRITTEN.   07/01/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP. STARTAS AV 4333.                 
001000*        SKAPAR DHL-ETIKETTER OCH UPPDATERAR WDR4                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T339X                                             
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100                                                                          
002200*    -- CHECKED BY WY2000                                                 
002300 77  IDPGM                       PIC X(08)   VALUE 'W4033900'.            
002400                                                                          
002500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002600 77  FELTEXT                     PIC X(80)        VALUE SPACE.            
002700                                                                          
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
003100 77  WS-RESULTAT                 PIC 9(10)   VALUE ZERO.                  
003200 77  WS-REST                     PIC 9       VALUE ZERO.                  
003300 77  WS-KONTONR-KLASS-0          PIC 9(9)    VALUE ZERO.                  
003400 77  WS-KONTONR-KLASS-1          PIC 9(9)    VALUE ZERO.                  
003500 77  WS-IDPRODNR                 PIC S9(7)   VALUE ZERO COMP-3.           
003600 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003700 77  WS-KVKOLLI                  PIC 9(3)    VALUE ZERO.                  
003800 77  SPAR-IDORDNR5               PIC 9(5)    VALUE ZERO.                  
003900 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77  MAX-TAB                     PIC 9(3)    VALUE 150.                   
004100                                                                          
004200 77  ALLT-SW                     PIC X.                                   
004300     88  ALLT-OK                             VALUE 'J'.                   
004400     88  ALLT-FEL                            VALUE 'N'.                   
004500 77  POST-HITTAD-SW              PIC X.                                   
004600     88  POST-HITTAD                         VALUE 'J'.                   
004700 77  REPL-SW                     PIC X.                                   
004800     88  REPL-OK                             VALUE 'J'.                   
004900                                                                          
005000 01  WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                   
005100 01  FILLER REDEFINES WS-VKORDBTO.                                        
005200     03 WS-KILO                  PIC 9(6).                                
005300     03 WS-HEKTO                 PIC 9.                                   
005400                                                                          
005500 01  WS-VLORDBTO                 PIC 9(4)V9(3) VALUE ZERO.                
005600 01  FILLER REDEFINES WS-VLORDBTO.                                        
005700     03 WS-VLM3                  PIC 9(4).                                
005800     03 WS-VLCM3                 PIC 9(3).                                
005900                                                                          
006000*****                                                                     
006100 01  WRAD                        PIC X(25).                               
006200 01  FUNNEN                      PIC X       VALUE 'N'.                   
006300 01  SIFFROR                     PIC X(10)   VALUE '0123456789'.          
006400 01  BLANDAT                     PIC X(35)   VALUE                        
006500     '0123456789ABCDEFGHIJKLMNOPRSTUVWXYZ'.                               
006600 01  SOLAR-1                     PIC X(10)   VALUE '¤¤¤¤¤¤¤¤¤¤'.          
006700 01  SOLAR-2                     PIC X(35)   VALUE                        
006800     '¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤'.                               
006900 01  SOL                         PIC X       VALUE '¤'.                   
007000 01  POS1                        PIC S9(4)       COMP.                    
007100 01  POS2                        PIC S9(4)       COMP.                    
007200 01  ANTAL-SOLAR                 PIC S9(4)       COMP.                    
007300 01  POSTNUMMER                  PIC X(5).                                
007400*****                                                                     
007500 01  WS-POSTNUMMER.                                                       
007600     03 WS-POSTNR-ISO            PIC X(2).                                
007700     03 WS-POSTNR-KOD            PIC X(5).                                
007800                                                                          
007900 01  WS-IDAWB.                                                            
008000     03 WS-IDAWB-1-9             PIC 9(9).                                
008100     03 WS-IDAWB-10              PIC 9(1).                                
008200     EJECT                                                                
008300 01  LISTVAL                     PIC X(8)    VALUE SPACE.                 
008400 01  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
008500     EJECT                                                                
008600*START OF DHL-LABEL *********************************************         
008700*    AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.          *         
008800*    ANV. FÖR ATT SKRIVA DHL-ETIKETT I FORMAT 10.5 CM BREDD OCH *         
008900*    21 CM HÖJD I CDC.                                          *         
009000*****************************************************************         
009100 01  FILLER           PIC X(24)  VALUE 'DHL-ETIKETT-TERMO'.               
009200*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
009300*                              LABELPOINT                                 
009400 01  DHL-LABEL-THERMO.                                                    
009500   03  DHL-RAD       PIC X(132)  VALUE SPACE.                             
009600                                                                          
009700   03  DHL-STYR-01.                                                       
009800     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
009900                                                                          
010000   03  DHL-STYR-91.                                                       
010100     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
010200                                                                          
010300   03  DHL-HEAD-BEGMT-RAD1.                                               
010400     05  FILLER      PIC X(25) VALUE '!F T N   90  300 L 1 1 5 '.         
010500     05  FILLER      PIC X(1)  VALUE '"'.                                 
010600     05  DHL-BEGMT-RAD1        PIC X(30)  VALUE SPACE.                    
010700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
010800                                                                          
010900   03  DHL-HEAD-BEGMT-RAD2.                                               
011000     05  FILLER      PIC X(25) VALUE '!F T N  110  300 L 1 1 5 '.         
011100     05  FILLER      PIC X(1)  VALUE '"'.                                 
011200     05  DHL-BEGMT-RAD2        PIC X(30)  VALUE SPACE.                    
011300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
011400                                                                          
011500   03  DHL-HEAD-ADGMT-GATA.                                               
011600     05  FILLER      PIC X(25) VALUE '!F T N  140  300 L 1 1 5 '.         
011700     05  FILLER      PIC X(1)  VALUE '"'.                                 
011800     05  DHL-ADGMT-GATA        PIC X(30)  VALUE SPACE.                    
011900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
012000                                                                          
012100   03  DHL-HEAD-ADGMT-PADR.                                               
012200     05  FILLER      PIC X(25) VALUE '!F T N  165  300 L 1 1 3 '.         
012300     05  FILLER      PIC X(1)  VALUE '"'.                                 
012400     05  DHL-ADGMT-PADR        PIC X(30)  VALUE SPACE.                    
012500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
012600                                                                          
012700   03  DHL-HEAD-ADGMT-LAND.                                               
012800     05  FILLER      PIC X(25) VALUE '!F T N  190  300 L 1 1 3 '.         
012900     05  FILLER      PIC X(1)  VALUE '"'.                                 
013000     05  DHL-ADGMT-LAND        PIC X(30)  VALUE SPACE.                    
013100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
013200                                                                          
013300   03  DHL-HEAD-TELNR.                                                    
013400     05  FILLER      PIC X(25) VALUE '!F T N  165  750 L 1 1 3 '.         
013500     05  FILLER      PIC X(1)  VALUE '"'.                                 
013600     05  DHL-HEAD-PHONE        PIC X(06)  VALUE 'PHONE:'.                 
013700     05  DHL-TELNR             PIC X(15)  VALUE SPACE.                    
013800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
013900                                                                          
014000   03  DHL-HEAD-FAXNR.                                                    
014100     05  FILLER      PIC X(25) VALUE '!F T N  190  750 L 1 1 3 '.         
014200     05  FILLER      PIC X(1)  VALUE '"'.                                 
014300     05  DHL-FAXNR-TEXT        PIC X(15)  VALUE 'FAXNR:'.                 
014400     05  DHL-FAXNR             PIC X(15)  VALUE SPACE.                    
014500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
014600                                                                          
014700   03  DHL-HEAD-AWB-BARCODE.                                              
014800     05  FILLER   PIC X(30) VALUE '!F C N  310  250 L 110 3 12 '.         
014900     05  FILLER                  PIC X(1)   VALUE '"'.                    
015000     05  DHL-IDAWB-SMALL-BARCODE PIC X(10)  VALUE ZERO.                   
015100     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
015200                                                                          
015300   03  DHL-AWB-TEXT.                                                      
015400     05  FILLER      PIC X(25) VALUE '!F T N  340  550 L 1 1 3 '.         
015500     05  FILLER                  PIC X(1)   VALUE '"'.                    
015600     05  DHL-IDAWB-TEXT          PIC X(10)  VALUE ZERO.                   
015700     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
015800*FROM                                                                     
015900   03  DHL-RUB-FROM.                                                      
016000     05  FILLER      PIC X(25) VALUE '!F T N  420  120 L 2 2 3 '.         
016100     05  FILLER      PIC X(11) VALUE '"FROM:"Å'.                          
016200                                                                          
016300   03  DHL-FROM-BEGMTRAD1.                                                
016400     05  FILLER      PIC X(25) VALUE '!F T N  450  120 L 2 1 7 '.         
016500     05  FILLER      PIC X(1)  VALUE '"'.                                 
016600     05  DHL-RAD1-FROM PIC X(29)                                          
016700         VALUE 'VOLVO CAR CUSTOMER SERVICE'.                              
016800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
016900                                                                          
017000   03  DHL-FROM-BEGMTRAD2.                                                
017100     05  FILLER      PIC X(25) VALUE '!F T N  480  120 L 2 1 7 '.         
017200     05  FILLER      PIC X(1)  VALUE '"'.                                 
017300     05  DHL-RAD2-FROM   PIC X(05)  VALUE '57540'.                        
017400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
017500                                                                          
017600   03  DHL-FROM-ADGMTGATA.                                                
017700     05  FILLER      PIC X(25) VALUE '!F T N  510  120 L 2 1 7 '.         
017800     05  FILLER      PIC X(1)  VALUE '"'.                                 
017900     05  DHL-GATA-FROM PIC X(22) VALUE 'ASSAR GABRIELSSONS VÄG'.          
018000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
018100                                                                          
018200   03  DHL-FROM-ADGMTPADR.                                                
018300     05  FILLER      PIC X(25) VALUE '!F T N  540  120 L 2 1 7 '.         
018400     05  FILLER      PIC X(1)  VALUE '"'.                                 
018500     05  DHL-PADR-FROM   PIC X(15)  VALUE '405 08 GÖTEBORG'.              
018600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
018700                                                                          
018800   03  DHL-FROM-ADGMTLAND.                                                
018900     05  FILLER      PIC X(25) VALUE '!F T N  570  120 L 2 1 7 '.         
019000     05  FILLER      PIC X(1)  VALUE '"'.                                 
019100     05  DHL-LAND-FROM   PIC X(30)  VALUE 'SWEDEN'.                       
019200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
019300                                                                          
019400   03  DHL-FROM-TELNR.                                                    
019500     05  FILLER      PIC X(25) VALUE '!F T N  600  120 L 2 1 7 '.         
019600     05  FILLER      PIC X(1)  VALUE '"'.                                 
019700     05  DHL-FROM-PHONE        PIC X(06)  VALUE "PHONE:".                 
019800     05  DHL-TELNR-FROM        PIC X(15)  VALUE '+46-31-590279'.          
019900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020000                                                                          
020100   03  DHL-FROM-FAXNR.                                                    
020200     05  FILLER      PIC X(25) VALUE '!F T N  630  120 L 2 1 7 '.         
020300     05  FILLER      PIC X(1)  VALUE '"'.                                 
020400     05  DHL-FROM-FAX          PIC X(04)  VALUE "FAX:".                   
020500     05  DHL-FAXNR-FROM        PIC X(15)  VALUE '+46-31-598585'.          
020600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020700                                                                          
020800   03  DHL-FROM-VAT-SPACE.                                                
020900     05  FILLER      PIC X(25) VALUE '!F T N  660  120 L 2 1 7 '.         
021000     05  FILLER      PIC X(1)  VALUE '"'.                                 
021100     05  FILLER                PIC X(07)   VALUE 'VAT NO:'.               
021200     05  DHL-VATNR             PIC Z(15)9  VALUE ZERO.                    
021300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
021400                                                                          
021500   03  DHL-FROM-ACCOUNTNR.                                                
021600     05  FILLER      PIC X(25) VALUE '!F T N  690  120 L 2 1 7 '.         
021700     05  FILLER      PIC X(1)  VALUE '"'.                                 
021800     05  FILLER                PIC X(11)   VALUE 'ACCOUNT NO:'.           
021900     05  DHL-ACCOUNTNR         PIC Z(15)   VALUE ZERO.                    
022000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
022100                                                                          
022200   03  DHL-FROM-REFNR.                                                    
022300     05  FILLER      PIC X(25) VALUE '!F T N  725  120 L 2 1 7 '.         
022400     05  FILLER      PIC X(1)  VALUE '"'.                                 
022500     05  FILLER                PIC X(04)   VALUE 'REF:'.                  
022600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
022700                                                                          
022800   03  DHL-FROM-IDKUND-IDORD.                                             
022900     05  FILLER      PIC X(25) VALUE '!F T N  725  170 L 3 1 1 '.         
023000     05  FILLER      PIC X(1)  VALUE '"'.                                 
023100     05  DHL-IDKUNDNR          PIC Z(07)   VALUE ZERO.                    
023200     05  FILLER                PIC X(01)   VALUE SPACE.                   
023300     05  FILLER                PIC X(01)   VALUE '*'.                     
023400     05  FILLER                PIC X(01)   VALUE SPACE.                   
023500     05  DHL-IDORDNR5          PIC Z(05)   VALUE ZERO.                    
023600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
023700                                                                          
023800   03  DHL-FROM-SHIP-DATE.                                                
023900     05  FILLER      PIC X(25) VALUE '!F T N  750  120 L 2 1 7 '.         
024000     05  FILLER      PIC X(1)  VALUE '"'.                                 
024100     05  FILLER                PIC X(11)   VALUE 'SHIP DATE: '.           
024200     05  DHL-SHIPDATE-AA       PIC 9(02)   VALUE ZERO.                    
024300     05  FILLER                PIC X(01)   VALUE '-'.                     
024400     05  DHL-SHIPDATE-MM       PIC 9(02)   VALUE ZERO.                    
024500     05  FILLER                PIC X(01)   VALUE '-'.                     
024600     05  DHL-SHIPDATE-DD       PIC 9(02)   VALUE ZERO.                    
024700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024800*TO                                                                       
024900   03  DHL-RUB-TO.                                                        
025000     05  FILLER      PIC X(25) VALUE '!F T N  430  520 L 2 2 3 '.         
025100     05  FILLER      PIC X(11) VALUE '"TO:"Å'.                            
025200                                                                          
025300   03  DHL-TO-BEGMTRAD1.                                                  
025400     05  FILLER      PIC X(25) VALUE '!F T N  460  520 L 1 1 3 '.         
025500     05  FILLER      PIC X(1)  VALUE '"'.                                 
025600     05  DHL-TO-BEGMT-RAD1     PIC X(30)  VALUE ZERO.                     
025700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025800                                                                          
025900   03  DHL-TO-BEGMTRAD2.                                                  
026000     05  FILLER      PIC X(25) VALUE '!F T N  490  520 L 1 1 3 '.         
026100     05  FILLER      PIC X(1)  VALUE '"'.                                 
026200     05  DHL-TO-BEGMT-RAD2     PIC X(30)  VALUE ZERO.                     
026300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
026400                                                                          
026500   03  DHL-TO-ADGMTGATA.                                                  
026600     05  FILLER      PIC X(25) VALUE '!F T N  520  520 L 1 1 3 '.         
026700     05  FILLER      PIC X(1)  VALUE '"'.                                 
026800     05  DHL-TO-ADGMT-GATA     PIC X(30)  VALUE ZERO.                     
026900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
027000                                                                          
027100   03  DHL-TO-ADGMTPADR.                                                  
027200     05  FILLER      PIC X(25) VALUE '!F T N  550  520 L 1 1 3 '.         
027300     05  FILLER      PIC X(1)  VALUE '"'.                                 
027400     05  DHL-TO-ADGMT-PADR     PIC X(30)  VALUE ZERO.                     
027500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
027600                                                                          
027700   03  DHL-TO-ADGMTLAND.                                                  
027800     05  FILLER      PIC X(25) VALUE '!F T N  600  520 L 2 1 3 '.         
027900     05  FILLER      PIC X(1)  VALUE '"'.                                 
028000     05  DHL-TO-ADGMT-LAND     PIC X(30)  VALUE ZERO.                     
028100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028200                                                                          
028300   03  DHL-TO-TELNR.                                                      
028400     05  FILLER      PIC X(25) VALUE '!F T N  640  520 L 1 1 3 '.         
028500     05  FILLER      PIC X(1)  VALUE '"'.                                 
028600     05  DHL-TO-PHONE          PIC X(07)  VALUE "PHONE: ".                
028700     05  DHL-TO-TEL-NR         PIC X(13)  VALUE "             ".          
028800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028900                                                                          
029000   03  DHL-TO-FAXNR.                                                      
029100     05  FILLER      PIC X(25) VALUE '!F T N  670  520 L 1 1 3 '.         
029200     05  FILLER      PIC X(1)  VALUE '"'.                                 
029300     05  DHL-TO-FAX            PIC X(05)  VALUE 'FAX: '.                  
029400     05  DHL-TO-FAX-NR         PIC X(14)  VALUE '              '.         
029500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
029600                                                                          
029700   03  DHL-TO-VAT-SPACE.                                                  
029800     05  FILLER      PIC X(25) VALUE '!F T N  700  520 L 1 1 3 '.         
029900     05  FILLER      PIC X(1)  VALUE '"'.                                 
030000     05  FILLER                PIC X(07)   VALUE 'VAT NO:'.               
030100     05  DHL-TO-VATNR          PIC Z(15)9  VALUE ZERO.                    
030200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
030300*IMPORT/EXPORT                                                            
030400                                                                          
030500   03  DHL-IMPORT-EXPORT-TYPE.                                            
030600     05  FILLER      PIC X(25) VALUE '!F T N  805  120 L 3 1 1 '.         
030700     05  FILLER      PIC X(1)  VALUE '"'.                                 
030800     05  FILLER      PIC X(19)   VALUE 'IMPORT/EXPORT TYPE:'.             
030900     05  DHL-IMPORT-EXPORT     PIC X(02)  VALUE ' P'.                     
031000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
031100                                                                          
031200   03  DHL-VALUE.                                                         
031300     05  FILLER      PIC X(25) VALUE '!F T N  840  350 R 3 1 1 '.         
031400     05  FILLER      PIC X(1)  VALUE '"'.                                 
031500     05  FILLER      PIC X(18) VALUE 'VALUE:            '.                
031600     05  DHL-VALUE-0           PIC Z(02)9  VALUE ZERO.                    
031700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
031800                                                                          
031900   03  DHL-WEIGHT.                                                        
032000     05  FILLER      PIC X(25) VALUE '!F T N  875  350 R 3 1 1 '.         
032100     05  FILLER      PIC X(1)  VALUE '"'.                                 
032200     05  FILLER      PIC X(15) VALUE 'WEIGHT:        '.                   
032300     05  DHL-KILO    PIC Z(4)9 VALUE ZERO.                                
032400     05  DHL-KOMMA   PIC X     VALUE ','.                                 
032500     05  DHL-HEKTO   PIC 9     VALUE ZERO.                                
032600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
032700                                                                          
032800   03  DHL-DIM-WEIGHT.                                                    
032900     05  FILLER      PIC X(25) VALUE '!F T N  910  350 R 3 1 1 '.         
033000     05  FILLER      PIC X(1)  VALUE '"'.                                 
033100     05  FILLER      PIC X(16) VALUE 'DIM.WEIGHT:     '.                  
033200     05  DHL-VLM3              PIC Z(03)9  VALUE ZERO.                    
033300     05  DHL-KOMMA-M3          PIC X       VALUE ','.                     
033400     05  DHL-VLCM3             PIC 9(3)    VALUE ZERO.                    
033500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
033600                                                                          
033700   03  DHL-DIMENSIONS-TEXT.                                               
033800     05  FILLER      PIC X(25) VALUE '!F T N  950  120 L 3 1 1 '.         
033900     05  FILLER      PIC X(1)  VALUE '"'.                                 
034000     05  FILLER      PIC X(11)   VALUE 'DIMENSIONS:'.                     
034100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
034200                                                                          
034300   03  DHL-DIMENSIONS.                                                    
034400     05  FILLER      PIC X(25) VALUE '!F T N  990  120 L 3 1 1 '.         
034500     05  FILLER      PIC X(1)  VALUE '"'.                                 
034600     05  DHL-DIKOLLIL          PIC Z(05)9  VALUE ZERO.                    
034700     05  FILLER                PIC X(03)   VALUE ' X '.                   
034800     05  DHL-DIKOLLIB          PIC Z(05)9  VALUE ZERO.                    
034900     05  FILLER                PIC X(03)   VALUE ' X '.                   
035000     05  DHL-DIKOLLIH          PIC Z(05)9  VALUE ZERO.                    
035100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
035200*DESCRIPTION                                                              
035300   03  DHL-DESCRIPTION.                                                   
035400     05  FILLER      PIC X(25) VALUE '!F T N  780  520 L 3 1 1 '.         
035500     05  FILLER      PIC X(1)  VALUE '"'.                                 
035600     05  FILLER      PIC X(13) VALUE 'DESCRIPTION: '.                     
035700     05  FILLER      PIC X(11) VALUE 'SPARE PARTS'.                       
035800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
035900                                                                          
036000   03  DHL-SERVICE.                                                       
036100     05  FILLER      PIC X(25) VALUE '!F T N  830  520 L 3 1 1 '.         
036200     05  FILLER      PIC X(1)  VALUE '"'.                                 
036300     05  FILLER      PIC X(13) VALUE 'SERVICE'.                           
036400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
036500*WARSAW CONVENTION AND DHL TERMS & CONDITIONS.                            
036600   03  DHL-DHL-TERMS1.                                                    
036700     05  FILLER      PIC X(25) VALUE '!F T N  890  520 L 1 1 4 '.         
036800     05  FILLER      PIC X(1)  VALUE '"'.                                 
036900     05  FILLER       PIC X(40) VALUE 'DHL STANDARD TERMS AND COND        
037000-        'ITIONS APPLY'.                                                  
037100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
037200                                                                          
037300   03  DHL-DHL-TERMS2.                                                    
037400     05  FILLER      PIC X(25) VALUE '!F T N  900  520 L 1 1 4 '.         
037500     05  FILLER      PIC X(1)  VALUE '"'.                                 
037600     05  FILLER      PIC X(34) VALUE 'WARSAW CONVENTION MAY ALSO A        
037700-        'PPLY.'.                                                         
037800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
037900                                                                          
038000   03  DHL-DHL-TERMS3.                                                    
038100     05  FILLER      PIC X(25) VALUE '!F T N  910  520 L 1 1 4 '.         
038200     05  FILLER      PIC X(1)  VALUE '"'.                                 
038300     05  FILLER     PIC X(40) VALUE 'SHIPMENT MAY BE CARRIED VIA I        
038400-        'NTERMEDIATE'.                                                   
038500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
038600                                                                          
038700   03  DHL-DHL-TERMS4.                                                    
038800     05  FILLER      PIC X(25) VALUE '!F T N  920  520 L 1 1 4 '.         
038900     05  FILLER      PIC X(1)  VALUE '"'.                                 
039000     05  FILLER      PIC X(44) VALUE 'STOPPING PLACES WHICH DHL DE        
039100-        'EMS APPROPRIATE.'.                                              
039200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
039300                                                                          
039400*PRODUCT                                                                  
039500   03  DHL-PRODUCT.                                                       
039600     05  FILLER      PIC X(25) VALUE '!F T N 1090  120 L 2 1 3 '.         
039700     05  FILLER      PIC X(1)  VALUE '"'.                                 
039800     05  FILLER      PIC X(09) VALUE 'PRODUCT: '.                         
039900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
040000                                                                          
040100   03  DHL-LINE-TOP.                                                      
040200     05  FILLER      PIC X(25) VALUE '!F B N  980  500 L  8 400'.         
040300                                                                          
040400   03  DHL-LINE-LEFT.                                                     
040500     05  FILLER      PIC X(25) VALUE '!F B N 1130  500 L 150  8'.         
040600                                                                          
040700   03  DHL-LINE-RIGHT.                                                    
040800     05  FILLER      PIC X(25) VALUE '!F B N 1130  900 L 150  8'.         
040900                                                                          
041000   03  DHL-LINE-BOTTOM.                                                   
041100     05  FILLER      PIC X(25) VALUE '!F B N 1130  500 L  8 400'.         
041200                                                                          
041300   03  DHL-ECX.                                                           
041400     05  FILLER      PIC X(25) VALUE '!F T N 1110  540 L 6 6 3 '.         
041500     05  FILLER      PIC X(1)  VALUE '"'.                                 
041600     05  FILLER      PIC X(03) VALUE 'ECT'.                               
041700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
041800                                                                          
041900*DESTINATION                                                              
042000   03  DHL-DESTINATION.                                                   
042100     05  FILLER      PIC X(25) VALUE '!F T N 1200  120 L 2 1 3 '.         
042200     05  FILLER      PIC X(1)  VALUE '"'.                                 
042300     05  FILLER      PIC X(13) VALUE 'DESTINATION: '.                     
042400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
042500                                                                          
042600   03  DHL-CITY.                                                          
042700     05  FILLER      PIC X(25) VALUE '!F T N 1350  120 L 6 5 3 '.         
042800     05  FILLER      PIC X(1)  VALUE '"'.                                 
042900     05  DHL-IDCITY  PIC X(03) VALUE SPACE.                               
043000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
043100                                                                          
043200   03  DHL-DESTINATION-BARCODE.                                           
043300     05  FILLER   PIC X(30) VALUE '!F C N 1400  470 L 250 3 12 '.         
043400     05  FILLER                  PIC X(1)  VALUE '"'.                     
043500     05  FILLER                  PIC X(3)  VALUE 'GOT'.                   
043600     05  DHL-DEST-IDCITY         PIC X(3)  VALUE SPACE.                   
043700     05  FILLER                  PIC X(1)  VALUE 'U'.                     
043800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
043900*AWB                                                                      
044000   03  DHL-AWB.                                                           
044100     05  FILLER      PIC X(25) VALUE '!F T N 1460  120 L 1 1 6 '.         
044200     05  FILLER      PIC X(1)  VALUE '"'.                                 
044300     05  FILLER      PIC X(08) VALUE 'AWB:    '.                          
044400     05  DHL-IDAWB   PIC X(10)  VALUE ZERO.                               
044500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
044600                                                                          
044700   03  DHL-AWB-BARCODE.                                                   
044800     05  FILLER   PIC X(30) VALUE '!F C N 1750  220 L 250 3 12 '.         
044900     05  FILLER                  PIC X(1)   VALUE '"'.                    
045000     05  DHL-IDAWB-BIG-BARCODE   PIC X(10)  VALUE ZERO.                   
045100     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
045200                                                                          
045300   03  DHL-TXT-NON-NEGOTIABLE.                                            
045400     05  FILLER      PIC X(25) VALUE '!F T N 1490  120 L 2 1 7 '.         
045500     05  FILLER      PIC X(1)  VALUE '"'.                                 
045600     05  FILLER      PIC X(16) VALUE '(NON-NEGOTIABLE)'.                  
045700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
045800                                                                          
045900   03  DHL-WEB-TEXT.                                                      
046000     05  FILLER      PIC X(25) VALUE '!F T N 1785  120 L 2 1 7 '.         
046100     05  FILLER      PIC X(1)  VALUE '"'.                                 
046200     05  FILLER   PIC X(28) VALUE 'WEBSITE: HTTP://WWW.DHL.COM'.          
046300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
046400*IDKOLLI-ORIGIN                                                           
046500   03  DHL-KOLLI.                                                         
046600     05  FILLER      PIC X(25) VALUE '!F T N 1810  700 L 2 1 3 '.         
046700     05  FILLER      PIC X(1)  VALUE '"'.                                 
046800     05  DHL-ANTAL   PIC Z(03) VALUE ZERO.                                
046900     05  FILLER      PIC X(04) VALUE ' OF '.                              
047000     05  DHL-TOTAL   PIC Z(03) VALUE ZERO.                                
047100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
047200                                                                          
047300   03  DHL-ORIGIN.                                                        
047400     05  FILLER      PIC X(25) VALUE '!F T N 1870  700 L 2 1 3 '.         
047500     05  FILLER      PIC X(1)  VALUE '"'.                                 
047600     05  FILLER      PIC X(09) VALUE 'ORIGIN:  '.                         
047700     05  FILLER      PIC X(03) VALUE 'GOT'.                               
047800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
047900*3 SMALL LABELS AT BOTTOM OF DHL ETIKETT.                                 
048000*LABEL1                                                                   
048100   03  DHL-31-BEGMTRAD1.                                                  
048200     05  FILLER      PIC X(25) VALUE '!F T N 2020  120 L 2 1 7 '.         
048300     05  FILLER      PIC X(1)  VALUE '"'.                                 
048400     05  FILLER      PIC X(03) VALUE 'TO:'.                               
048500     05  DHL-31-BEGMT-RAD1     PIC X(30)  VALUE SPACE.                    
048600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
048700                                                                          
048800   03  DHL-31-BEGMTRAD2.                                                  
048900     05  FILLER      PIC X(25) VALUE '!F T N 2040  120 L 2 1 7 '.         
049000     05  FILLER      PIC X(1)  VALUE '"'.                                 
049100     05  DHL-31-BEGMT-RAD2     PIC X(30)  VALUE SPACE.                    
049200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
049300                                                                          
049400   03  DHL-31-ADGMTLAND.                                                  
049500     05  FILLER      PIC X(25) VALUE '!F T N 2060  120 L 2 1 7 '.         
049600     05  FILLER      PIC X(1)  VALUE '"'.                                 
049700     05  DHL-31-ADGMT-LAND     PIC X(30)  VALUE SPACE.                    
049800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
049900                                                                          
050000   03  DHL-31-DATE.                                                       
050100     05  FILLER      PIC X(25) VALUE '!F T N 1995  120 L 2 1 7 '.         
050200     05  FILLER      PIC X(1)  VALUE '"'.                                 
050300     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
050400     05  DHL-31-SHIPDATE-AA    PIC 9(02)   VALUE ZERO.                    
050500     05  FILLER                PIC X(01)   VALUE '-'.                     
050600     05  DHL-31-SHIPDATE-MM    PIC 9(02)   VALUE ZERO.                    
050700     05  FILLER                PIC X(01)   VALUE '-'.                     
050800     05  DHL-31-SHIPDATE-DD    PIC 9(02)   VALUE ZERO.                    
050900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
051000                                                                          
051100   03  DHL-31-AWB.                                                        
051200     05  FILLER      PIC X(25) VALUE '!F T N 2105 100 L 2 1 3 '.          
051300     05  FILLER      PIC X(1)  VALUE '"'.                                 
051400     05  FILLER      PIC X(05) VALUE 'AWB:'.                              
051500     05  DHL-31-IDAWB    PIC X(10)  VALUE ZERO.                           
051600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
051700                                                                          
051800*LABEL2                                                                   
051900   03  DHL-32-BEGMTRAD1.                                                  
052000     05  FILLER      PIC X(25) VALUE '!F T N 2020  450 L 2 1 7 '.         
052100     05  FILLER      PIC X(1)  VALUE '"'.                                 
052200     05  FILLER                PIC X(03)  VALUE 'TO:'.                    
052300     05  DHL-32-BEGMT-RAD1     PIC X(30)  VALUE SPACE.                    
052400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
052500                                                                          
052600   03  DHL-32-BEGMTRAD2.                                                  
052700     05  FILLER      PIC X(25) VALUE '!F T N 2040  450 L 2 1 7 '.         
052800     05  FILLER      PIC X(1)  VALUE '"'.                                 
052900     05  DHL-32-BEGMT-RAD2     PIC X(30)  VALUE SPACE.                    
053000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
053100                                                                          
053200   03  DHL-32-ADGMTLAND.                                                  
053300     05  FILLER      PIC X(25) VALUE '!F T N 2060  450 L 2 1 7 '.         
053400     05  FILLER      PIC X(1)  VALUE '"'.                                 
053500     05  DHL-32-ADGMT-LAND     PIC X(30)  VALUE SPACE.                    
053600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
053700                                                                          
053800   03  DHL-32-DATE.                                                       
053900     05  FILLER      PIC X(25) VALUE '!F T N 1995  450 L 2 1 7 '.         
054000     05  FILLER      PIC X(1)  VALUE '"'.                                 
054100     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
054200     05  DHL-32-SHIPDATE-AA    PIC 9(02)   VALUE ZERO.                    
054300     05  FILLER                PIC X(01)   VALUE '-'.                     
054400     05  DHL-32-SHIPDATE-MM    PIC 9(02)   VALUE ZERO.                    
054500     05  FILLER                PIC X(01)   VALUE '-'.                     
054600     05  DHL-32-SHIPDATE-DD    PIC 9(02)   VALUE ZERO.                    
054700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
054800                                                                          
054900   03  DHL-32-AWB.                                                        
055000     05  FILLER      PIC X(25) VALUE '!F T N 2105  450 L 2 1 3 '.         
055100     05  FILLER      PIC X(1)  VALUE '"'.                                 
055200     05  FILLER      PIC X(05) VALUE 'AWB: '.                             
055300     05  DHL-32-IDAWB    PIC X(10)  VALUE ZERO.                           
055400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
055500*LABEL3                                                                   
055600   03  DHL-33-BEGMTRAD1.                                                  
055700     05  FILLER      PIC X(25) VALUE '!F T N 2020  780 L 2 1 7 '.         
055800     05  FILLER      PIC X(1)  VALUE '"'.                                 
055900     05  FILLER                PIC X(03)  VALUE 'TO:'.                    
056000     05  DHL-33-BEGMT-RAD1     PIC X(30)  VALUE SPACE.                    
056100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
056200                                                                          
056300   03  DHL-33-BEGMTRAD2.                                                  
056400     05  FILLER      PIC X(25) VALUE '!F T N 2040  780 L 2 1 7 '.         
056500     05  FILLER      PIC X(1)  VALUE '"'.                                 
056600     05  DHL-33-BEGMT-RAD2     PIC X(30)  VALUE SPACE.                    
056700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
056800                                                                          
056900   03  DHL-33-ADGMTLAND.                                                  
057000     05  FILLER      PIC X(25) VALUE '!F T N 2060  780 L 2 1 7 '.         
057100     05  FILLER      PIC X(1)  VALUE '"'.                                 
057200     05  DHL-33-ADGMT-LAND     PIC X(30)  VALUE SPACE.                    
057300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
057400                                                                          
057500   03  DHL-33-DATE.                                                       
057600     05  FILLER      PIC X(25) VALUE '!F T N 1995  780 L 2 1 7 '.         
057700     05  FILLER      PIC X(1)  VALUE '"'.                                 
057800     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
057900     05  DHL-33-SHIPDATE-AA    PIC 9(02)   VALUE ZERO.                    
058000     05  FILLER                PIC X(01)   VALUE '-'.                     
058100     05  DHL-33-SHIPDATE-MM    PIC 9(02)   VALUE ZERO.                    
058200     05  FILLER                PIC X(01)   VALUE '-'.                     
058300     05  DHL-33-SHIPDATE-DD    PIC 9(02)   VALUE ZERO.                    
058400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
058500                                                                          
058600   03  DHL-33-AWB.                                                        
058700     05  FILLER      PIC X(25) VALUE '!F T N 2105  780 L 2 1 3 '.         
058800     05  FILLER      PIC X(1)  VALUE '"'.                                 
058900     05  FILLER      PIC X(05) VALUE 'AWB: '.                             
059000     05  DHL-33-IDAWB    PIC X(10)  VALUE ZERO.                           
059100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
059200                                                                          
059300*END OF DHL-LABEL *****************************************               
059400*                                                                         
059500*NOVA                                                                     
059600*NOVA                                                                     
059700*NOVA                                                                     
059800*START OF NOVA DHL-LABEL ***NOVA***NOVA***NOVA***NOVA************         
059900*    AREA MED STYRTECKEN FÖR NOVA      TERMO SKRIVARE.          *         
060000*    ANV. FÖR ATT SKRIVA DHL-ETIKETT I FORMAT 10.5 CM BREDD OCH *         
060100*    21 CM HÖJD I CDC.                                          *         
060200*****************************************************************         
060300 01  FILLER           PIC X(24)  VALUE 'NOVA-DHL-LABEL-TERMO'.            
060400*    STYRTECKEN ENLIGT MANUAL: NOVA THERMAL PRINTER                       
060500*                                                                         
060600 01  NOVA-DHL-LABEL-THERMO.                                               
060700   03  NOVA-DHL-RAD  PIC X(132)  VALUE SPACE.                             
060800                                                                          
060900   03  NOVA-DHL-STYR-01.                                                  
061000     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
061100                                                                          
061200   03  DHL-STYR-42.                                                       
061300     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
061400                                                                          
061500   03  NOVA-DHL-STYR-91.                                                  
061600     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
061700                                                                          
061800   03  NOVA-DHL-HEAD-BEGMT-RAD1.                                          
061900     05  FILLER      PIC X(25) VALUE '!F T N   25  200 L 1 1 5 '.         
062000     05  FILLER      PIC X(1)  VALUE '"'.                                 
062100     05  NOVA-DHL-BEGMT-RAD1   PIC X(30)  VALUE SPACE.                    
062200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
062300                                                                          
062400   03  NOVA-DHL-HEAD-BEGMT-RAD2.                                          
062500     05  FILLER      PIC X(25) VALUE '!F T N   55  200 L 1 1 3 '.         
062600     05  FILLER      PIC X(1)  VALUE '"'.                                 
062700     05  NOVA-DHL-BEGMT-RAD2   PIC X(30)  VALUE SPACE.                    
062800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
062900                                                                          
063000   03  NOVA-DHL-HEAD-ADGMT-GATA.                                          
063100     05  FILLER      PIC X(25) VALUE '!F T N   80  200 L 1 1 3 '.         
063200     05  FILLER      PIC X(1)  VALUE '"'.                                 
063300     05  NOVA-DHL-ADGMT-GATA   PIC X(30)  VALUE SPACE.                    
063400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
063500                                                                          
063600   03  NOVA-DHL-HEAD-ADGMT-PADR.                                          
063700     05  FILLER      PIC X(25) VALUE '!F T N  105  200 L 1 1 3 '.         
063800     05  FILLER      PIC X(1)  VALUE '"'.                                 
063900     05  NOVA-DHL-ADGMT-PADR   PIC X(30)  VALUE SPACE.                    
064000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
064100                                                                          
064200   03  NOVA-DHL-HEAD-ADGMT-LAND.                                          
064300     05  FILLER      PIC X(25) VALUE '!F T N  130  200 L 1 1 3 '.         
064400     05  FILLER      PIC X(1)  VALUE '"'.                                 
064500     05  NOVA-DHL-ADGMT-LAND   PIC X(30)  VALUE SPACE.                    
064600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
064700                                                                          
064800   03  NOVA-DHL-HEAD-TELNR.                                               
064900     05  FILLER      PIC X(25) VALUE '!F T N  105  780 L 1 1 3 '.         
065000     05  FILLER      PIC X(1)  VALUE '"'.                                 
065100     05  NOVA-DHL-HEAD-PHONE   PIC X(06)  VALUE 'PHONE:'.                 
065200     05  NOVA-DHL-TELNR        PIC X(15)  VALUE SPACE.                    
065300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
065400                                                                          
065500   03  NOVA-DHL-HEAD-FAXNR.                                               
065600     05  FILLER      PIC X(25) VALUE '!F T N  130  780 L 1 1 3 '.         
065700     05  FILLER      PIC X(1)  VALUE '"'.                                 
065800     05  NOVA-DHL-FAXNR-TEXT   PIC X(15)  VALUE 'FAXNR:'.                 
065900     05  NOVA-DHL-FAXNR        PIC X(15)  VALUE SPACE.                    
066000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
066100                                                                          
066200   03  NOVA-DHL-HEAD-AWB-BARCODE.                                         
066300     05  FILLER   PIC X(30) VALUE '!F C N  250  200 L 110 3 12 '.         
066400     05  FILLER                  PIC X(1)   VALUE '"'.                    
066500     05  NOVA-DHL-IDAWB-SMALL-BARCODE PIC X(10) VALUE ZERO.               
066600     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
066700                                                                          
066800   03  NOVA-DHL-AWB-TEXT.                                                 
066900     05  FILLER      PIC X(25) VALUE '!F T N  280  450 L 1 1 3 '.         
067000     05  FILLER                  PIC X(1)   VALUE '"'.                    
067100     05  NOVA-DHL-IDAWB-TEXT     PIC X(10)  VALUE ZERO.                   
067200     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
067300*FROM                                                                     
067400   03  NOVA-DHL-RUB-FROM.                                                 
067500     05  FILLER      PIC X(25) VALUE '!F T N  330  120 L 2 2 3 '.         
067600     05  FILLER      PIC X(11) VALUE '"FROM:"Å'.                          
067700                                                                          
067800   03  NOVA-DHL-FROM-BEGMTRAD1.                                           
067900     05  FILLER      PIC X(25) VALUE '!F T N  390  120 L 1 1 3 '.         
068000     05  FILLER      PIC X(1)  VALUE '"'.                                 
068100     05  NOVA-DHL-RAD1-FROM PIC X(29)                                     
068200         VALUE 'VOLVO CAR CUSTOMER SERVICE'.                              
068300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
068400                                                                          
068500   03  NOVA-DHL-FROM-BEGMTRAD2.                                           
068600     05  FILLER      PIC X(25) VALUE '!F T N  420  120 L 1 1 3 '.         
068700     05  FILLER      PIC X(1)  VALUE '"'.                                 
068800     05  NOVA-DHL-RAD2-FROM PIC X(05) VALUE '57540'.                      
068900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
069000                                                                          
069100   03  NOVA-DHL-FROM-ADGMTGATA.                                           
069200     05  FILLER      PIC X(25) VALUE '!F T N  450  120 L 1 1 3 '.         
069300     05  FILLER      PIC X(1)  VALUE '"'.                                 
069400     05  NOVA-DHL-GATA-FROM PIC X(22)                                     
069500         VALUE 'ASSAR GABRIELSSONS VÄG'.                                  
069600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
069700                                                                          
069800   03  NOVA-DHL-FROM-ADGMTPADR.                                           
069900     05  FILLER      PIC X(25) VALUE '!F T N  480  120 L 1 1 3 '.         
070000     05  FILLER      PIC X(1)  VALUE '"'.                                 
070100     05  NOVA-DHL-PADR-FROM PIC X(15) VALUE '405 08 GÖTEBORG'.            
070200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
070300                                                                          
070400   03  NOVA-DHL-FROM-ADGMTLAND.                                           
070500     05  FILLER      PIC X(25) VALUE '!F T N  510  120 L 1 1 3 '.         
070600     05  FILLER      PIC X(1)  VALUE '"'.                                 
070700     05  NOVA-DHL-LAND-FROM PIC X(30) VALUE 'SWEDEN'.                     
070800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
070900                                                                          
071000   03  NOVA-DHL-FROM-TELNR.                                               
071100     05  FILLER      PIC X(25) VALUE '!F T N  540  120 L 1 1 3 '.         
071200     05  FILLER      PIC X(1)  VALUE '"'.                                 
071300     05  NOVA-DHL-FROM-PHONE   PIC X(06)  VALUE "PHONE:".                 
071400     05  NOVA-DHL-TELNR-FROM   PIC X(15)  VALUE '+46-31-590279'.          
071500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
071600                                                                          
071700   03  NOVA-DHL-FROM-FAXNR.                                               
071800     05  FILLER      PIC X(25) VALUE '!F T N  570  120 L 1 1 3 '.         
071900     05  FILLER      PIC X(1)  VALUE '"'.                                 
072000     05  NOVA-DHL-FROM-FAX     PIC X(04)  VALUE "FAX:".                   
072100     05  NOVA-DHL-FAXNR-FROM   PIC X(15)  VALUE '+46-31-598585'.          
072200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
072300                                                                          
072400   03  NOVA-DHL-FROM-VAT-SPACE.                                           
072500     05  FILLER      PIC X(25) VALUE '!F T N  600  120 L 1 1 3 '.         
072600     05  FILLER      PIC X(1)  VALUE '"'.                                 
072700     05  FILLER                PIC X(07)   VALUE 'VAT NO:'.               
072800     05  NOVA-DHL-VATNR        PIC Z(15)9  VALUE ZERO.                    
072900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
073000                                                                          
073100   03  NOVA-DHL-FROM-ACCOUNTNR.                                           
073200     05  FILLER      PIC X(25) VALUE '!F T N  630  120 L 1 1 3 '.         
073300     05  FILLER      PIC X(1)  VALUE '"'.                                 
073400     05  FILLER                PIC X(11)   VALUE 'ACCOUNT NO:'.           
073500     05  NOVA-DHL-ACCOUNTNR    PIC Z(15)   VALUE ZERO.                    
073600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
073700                                                                          
073800   03  NOVA-DHL-FROM-REFNR.                                               
073900     05  FILLER      PIC X(25) VALUE '!F T N  665  120 L 1 1 3 '.         
074000     05  FILLER      PIC X(1)  VALUE '"'.                                 
074100     05  FILLER                PIC X(04)   VALUE 'REF:'.                  
074200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
074300                                                                          
074400   03  NOVA-DHL-FROM-IDKUND-IDORD.                                        
074500     05  FILLER      PIC X(25) VALUE '!F T N  665  170 L 1 1 3 '.         
074600     05  FILLER      PIC X(1)  VALUE '"'.                                 
074700     05  NOVA-DHL-IDKUNDNR     PIC Z(07)   VALUE ZERO.                    
074800     05  FILLER                PIC X(01)   VALUE SPACE.                   
074900     05  FILLER                PIC X(01)   VALUE '*'.                     
075000     05  FILLER                PIC X(01)   VALUE SPACE.                   
075100     05  NOVA-DHL-IDORDNR5     PIC Z(05)   VALUE ZERO.                    
075200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
075300                                                                          
075400   03  NOVA-DHL-FROM-SHIP-DATE.                                           
075500     05  FILLER      PIC X(25) VALUE '!F T N  700  120 L 1 1 3 '.         
075600     05  FILLER      PIC X(1)  VALUE '"'.                                 
075700     05  FILLER                PIC X(11)   VALUE 'SHIP DATE: '.           
075800     05  NOVA-DHL-SHIPDATE-AA  PIC 9(02)   VALUE ZERO.                    
075900     05  FILLER                PIC X(01)   VALUE '-'.                     
076000     05  NOVA-DHL-SHIPDATE-MM  PIC 9(02)   VALUE ZERO.                    
076100     05  FILLER                PIC X(01)   VALUE '-'.                     
076200     05  NOVA-DHL-SHIPDATE-DD  PIC 9(02)   VALUE ZERO.                    
076300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
076400*TO                                                                       
076500   03  NOVA-DHL-RUB-TO.                                                   
076600     05  FILLER      PIC X(25) VALUE '!F T N  330  650 L 2 2 3 '.         
076700     05  FILLER      PIC X(11) VALUE '"TO:"Å'.                            
076800                                                                          
076900   03  NOVA-DHL-TO-BEGMTRAD1.                                             
077000     05  FILLER      PIC X(25) VALUE '!F T N  400  650 L 1 1 3 '.         
077100     05  FILLER      PIC X(1)  VALUE '"'.                                 
077200     05  NOVA-DHL-TO-BEGMT-RAD1 PIC X(30) VALUE ZERO.                     
077300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
077400                                                                          
077500   03  NOVA-DHL-TO-BEGMTRAD2.                                             
077600     05  FILLER      PIC X(25) VALUE '!F T N  430  650 L 1 1 3 '.         
077700     05  FILLER      PIC X(1)  VALUE '"'.                                 
077800     05  NOVA-DHL-TO-BEGMT-RAD2 PIC X(30) VALUE ZERO.                     
077900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
078000                                                                          
078100   03  NOVA-DHL-TO-ADGMTGATA.                                             
078200     05  FILLER      PIC X(25) VALUE '!F T N  460  650 L 1 1 3 '.         
078300     05  FILLER      PIC X(1)  VALUE '"'.                                 
078400     05  NOVA-DHL-TO-ADGMT-GATA PIC X(30) VALUE ZERO.                     
078500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
078600                                                                          
078700   03  NOVA-DHL-TO-ADGMTPADR.                                             
078800     05  FILLER      PIC X(25) VALUE '!F T N  490  650 L 1 1 3 '.         
078900     05  FILLER      PIC X(1)  VALUE '"'.                                 
079000     05  NOVA-DHL-TO-ADGMT-PADR PIC X(30) VALUE ZERO.                     
079100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
079200                                                                          
079300   03  NOVA-DHL-TO-ADGMTLAND.                                             
079400     05  FILLER      PIC X(25) VALUE '!F T N  520  650 L 1 1 3 '.         
079500     05  FILLER      PIC X(1)  VALUE '"'.                                 
079600     05  NOVA-DHL-TO-ADGMT-LAND PIC X(30) VALUE ZERO.                     
079700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
079800                                                                          
079900   03  NOVA-DHL-TO-TELNR.                                                 
080000     05  FILLER      PIC X(25) VALUE '!F T N  550  650 L 1 1 3 '.         
080100     05  FILLER      PIC X(1)  VALUE '"'.                                 
080200     05  NOVA-DHL-TO-PHONE     PIC X(07)  VALUE "PHONE: ".                
080300     05  NOVA-DHL-TO-TEL-NR    PIC X(13)  VALUE "             ".          
080400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
080500                                                                          
080600   03  NOVA-DHL-TO-FAXNR.                                                 
080700     05  FILLER      PIC X(25) VALUE '!F T N  580  650 L 1 1 3 '.         
080800     05  FILLER      PIC X(1)  VALUE '"'.                                 
080900     05  NOVA-DHL-TO-FAX       PIC X(05)  VALUE 'FAX: '.                  
081000     05  NOVA-DHL-TO-FAX-NR    PIC X(14)  VALUE '              '.         
081100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
081200                                                                          
081300   03  NOVA-DHL-TO-VAT-SPACE.                                             
081400     05  FILLER      PIC X(25) VALUE '!F T N  620  650 L 1 1 3 '.         
081500     05  FILLER      PIC X(1)  VALUE '"'.                                 
081600     05  FILLER                PIC X(07)   VALUE 'VAT NO:'.               
081700     05  NOVA-DHL-TO-VATNR     PIC Z(15)9  VALUE ZERO.                    
081800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
081900*IMPORT/EXPORT                                                            
082000                                                                          
082100   03  NOVA-DHL-IMPORT-EXPORT-TYPE.                                       
082200     05  FILLER      PIC X(25) VALUE '!F T N  745  120 L 3 1 1 '.         
082300     05  FILLER      PIC X(1)  VALUE '"'.                                 
082400     05  FILLER      PIC X(19)   VALUE 'IMPORT/EXPORT TYPE:'.             
082500     05  NOVA-DHL-IMPORT-EXPORT PIC X(02) VALUE ' P'.                     
082600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
082700                                                                          
082800   03  NOVA-DHL-VALUE.                                                    
082900     05  FILLER      PIC X(25) VALUE '!F T N  780  120 L 3 1 1 '.         
083000     05  FILLER      PIC X(1)  VALUE '"'.                                 
083100     05  FILLER      PIC X(18) VALUE 'VALUE:            '.                
083200     05  NOVA-DHL-VALUE-0      PIC Z(02)9  VALUE ZERO.                    
083300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
083400                                                                          
083500   03  NOVA-DHL-WEIGHT.                                                   
083600     05  FILLER      PIC X(25) VALUE '!F T N  815  120 L 3 1 1 '.         
083700     05  FILLER      PIC X(1)  VALUE '"'.                                 
083800     05  FILLER      PIC X(15) VALUE 'WEIGHT:        '.                   
083900     05  NOVA-DHL-KILO PIC Z(4)9 VALUE ZERO.                              
084000     05  NOVA-DHL-KOMMA PIC X  VALUE ','.                                 
084100     05  NOVA-DHL-HEKTO PIC 9  VALUE ZERO.                                
084200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
084300                                                                          
084400   03  NOVA-DHL-DIM-WEIGHT.                                               
084500     05  FILLER      PIC X(25) VALUE '!F T N  850  120 L 3 1 1 '.         
084600     05  FILLER      PIC X(1)  VALUE '"'.                                 
084700     05  FILLER      PIC X(16) VALUE 'DIM.WEIGHT:     '.                  
084800     05  NOVA-DHL-VLM3         PIC Z(03)9  VALUE ZERO.                    
084900     05  NOVA-DHL-KOMMA-M3     PIC X       VALUE ','.                     
085000     05  NOVA-DHL-VLCM3        PIC 9(3)    VALUE ZERO.                    
085100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
085200                                                                          
085300   03  NOVA-DHL-DIMENSIONS-TEXT.                                          
085400     05  FILLER      PIC X(25) VALUE '!F T N  890  120 L 3 1 1 '.         
085500     05  FILLER      PIC X(1)  VALUE '"'.                                 
085600     05  FILLER      PIC X(11)   VALUE 'DIMENSIONS:'.                     
085700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
085800                                                                          
085900   03  NOVA-DHL-DIMENSIONS.                                               
086000     05  FILLER      PIC X(25) VALUE '!F T N  940  120 L 3 1 1 '.         
086100     05  FILLER      PIC X(1)  VALUE '"'.                                 
086200     05  NOVA-DHL-DIKOLLIL     PIC Z(05)9  VALUE ZERO.                    
086300     05  FILLER                PIC X(03)   VALUE ' X '.                   
086400     05  NOVA-DHL-DIKOLLIB     PIC Z(05)9  VALUE ZERO.                    
086500     05  FILLER                PIC X(03)   VALUE ' X '.                   
086600     05  NOVA-DHL-DIKOLLIH     PIC Z(05)9  VALUE ZERO.                    
086700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
086800*DESCRIPTION                                                              
086900   03  NOVA-DHL-DESCRIPTION.                                              
087000     05  FILLER      PIC X(25) VALUE '!F T N  720  520 L 1 1 3 '.         
087100     05  FILLER      PIC X(1)  VALUE '"'.                                 
087200     05  FILLER      PIC X(13) VALUE 'DESCRIPTION: '.                     
087300     05  FILLER      PIC X(11) VALUE 'SPARE PARTS'.                       
087400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
087500                                                                          
087600   03  NOVA-DHL-SERVICE.                                                  
087700     05  FILLER      PIC X(25) VALUE '!F T N  760  520 L 1 1 3 '.         
087800     05  FILLER      PIC X(1)  VALUE '"'.                                 
087900     05  FILLER      PIC X(13) VALUE 'SERVICE'.                           
088000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
088100*WARSAW CONVENTION AND DHL TERMS & CONDITIONS.                            
088200   03  NOVA-DHL-DHL-TERMS1.                                               
088300     05  FILLER      PIC X(25) VALUE '!F T N  800  520 L 1 1 4 '.         
088400     05  FILLER      PIC X(1)  VALUE '"'.                                 
088500     05  FILLER       PIC X(40) VALUE 'DHL STANDARD TERMS AND COND        
088600-        'ITIONS APPLY'.                                                  
088700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
088800                                                                          
088900   03  NOVA-DHL-DHL-TERMS2.                                               
089000     05  FILLER      PIC X(25) VALUE '!F T N  820  520 L 1 1 4 '.         
089100     05  FILLER      PIC X(1)  VALUE '"'.                                 
089200     05  FILLER      PIC X(34) VALUE 'WARSAW CONVENTION MAY ALSO A        
089300-        'PPLY.'.                                                         
089400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
089500                                                                          
089600   03  NOVA-DHL-DHL-TERMS3.                                               
089700     05  FILLER      PIC X(25) VALUE '!F T N  840  520 L 1 1 4 '.         
089800     05  FILLER      PIC X(1)  VALUE '"'.                                 
089900     05  FILLER     PIC X(40) VALUE 'SHIPMENT MAY BE CARRIED VIA I        
090000-        'NTERMEDIATE'.                                                   
090100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
090200                                                                          
090300   03  NOVA-DHL-DHL-TERMS4.                                               
090400     05  FILLER      PIC X(25) VALUE '!F T N  860  520 L 1 1 4 '.         
090500     05  FILLER      PIC X(1)  VALUE '"'.                                 
090600     05  FILLER      PIC X(44) VALUE 'STOPPING PLACES WHICH DHL DE        
090700-        'EMS APPROPRIATE.'.                                              
090800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
090900                                                                          
091000*PRODUCT                                                                  
091100   03  NOVA-DHL-PRODUCT.                                                  
091200     05  FILLER      PIC X(25) VALUE '!F T N 1010  120 L 2 1 3 '.         
091300     05  FILLER      PIC X(1)  VALUE '"'.                                 
091400     05  FILLER      PIC X(09) VALUE 'PRODUCT: '.                         
091500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
091600                                                                          
091700   03  NOVA-DHL-LINE-TOP.                                                 
091800     05  FILLER      PIC X(25) VALUE '!F B N  890  500 L  8 400'.         
091900                                                                          
092000   03  NOVA-DHL-LINE-LEFT.                                                
092100     05  FILLER      PIC X(25) VALUE '!F B N 1020  500 L 140  8'.         
092200                                                                          
092300   03  NOVA-DHL-LINE-RIGHT.                                               
092400     05  FILLER      PIC X(25) VALUE '!F B N 1020  900 L 140  8'.         
092500                                                                          
092600   03  NOVA-DHL-LINE-BOTTOM.                                              
092700     05  FILLER      PIC X(25) VALUE '!F B N 1020  500 L  8 400'.         
092800                                                                          
092900   03  NOVA-DHL-ECX.                                                      
093000     05  FILLER      PIC X(25) VALUE '!F T N 1000  545 L 4 4 5 '.         
093100     05  FILLER      PIC X(1)  VALUE '"'.                                 
093200     05  FILLER      PIC X(03) VALUE 'ECT'.                               
093300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
093400                                                                          
093500*DESTINATION                                                              
093600   03  NOVA-DHL-DESTINATION.                                              
093700     05  FILLER      PIC X(25) VALUE '!F T N 1090  120 L 2 1 3 '.         
093800     05  FILLER      PIC X(1)  VALUE '"'.                                 
093900     05  FILLER      PIC X(13) VALUE 'DESTINATION: '.                     
094000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
094100                                                                          
094200   03  NOVA-DHL-CITY.                                                     
094300     05  FILLER      PIC X(25) VALUE '!F T N 1240  120 L 2 2 6 '.         
094400     05  FILLER      PIC X(1)  VALUE '"'.                                 
094500     05  NOVA-DHL-IDCITY PIC X(03) VALUE SPACE.                           
094600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
094700                                                                          
094800   03  NOVA-DHL-DESTINATION-BARCODE.                                      
094900     05  FILLER   PIC X(30) VALUE '!F C N 1290  450 L 250 3 12 '.         
095000     05  FILLER                  PIC X(1)  VALUE '"'.                     
095100     05  FILLER                  PIC X(3)  VALUE 'GOT'.                   
095200     05  NOVA-DHL-DEST-IDCITY    PIC X(3)  VALUE SPACE.                   
095300     05  FILLER                  PIC X(1)  VALUE 'U'.                     
095400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
095500*AWB                                                                      
095600   03  NOVA-DHL-AWB.                                                      
095700     05  FILLER      PIC X(25) VALUE '!F T N 1340  120 L 2 2 3 '.         
095800     05  FILLER      PIC X(1)  VALUE '"'.                                 
095900     05  FILLER      PIC X(08) VALUE 'AWB:    '.                          
096000     05  NOVA-DHL-IDAWB PIC X(10) VALUE ZERO.                             
096100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
096200                                                                          
096300   03  NOVA-DHL-AWB-BARCODE.                                              
096400     05  FILLER   PIC X(30) VALUE '!F C N 1640  250 L 250 3 12 '.         
096500     05  FILLER                  PIC X(1)   VALUE '"'.                    
096600     05  NOVA-DHL-IDAWB-BIG-BARCODE PIC X(10) VALUE ZERO.                 
096700     05  FILLER                  PIC X(2)   VALUE '"Å'.                   
096800                                                                          
096900   03  NOVA-DHL-TXT-NON-NEGOTIABLE.                                       
097000     05  FILLER      PIC X(25) VALUE '!F T N 1380  120 L 3 1 1 '.         
097100     05  FILLER      PIC X(1)  VALUE '"'.                                 
097200     05  FILLER      PIC X(16) VALUE '(NON-NEGOTIABLE)'.                  
097300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
097400                                                                          
097500   03  NOVA-DHL-WEB-TEXT.                                                 
097600     05  FILLER      PIC X(25) VALUE '!F T N 1710  120 L 3 1 1 '.         
097700     05  FILLER      PIC X(1)  VALUE '"'.                                 
097800     05  FILLER   PIC X(28) VALUE 'WEBSITE: HTTP://WWW.DHL.COM'.          
097900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
098000*IDKOLLI-ORIGIN                                                           
098100   03  NOVA-DHL-KOLLI.                                                    
098200     05  FILLER      PIC X(25) VALUE '!F T N 1690  700 L 2 1 3 '.         
098300     05  FILLER      PIC X(1)  VALUE '"'.                                 
098400     05  NOVA-DHL-ANTAL PIC Z(03) VALUE ZERO.                             
098500     05  FILLER      PIC X(04) VALUE ' OF '.                              
098600     05  NOVA-DHL-TOTAL PIC Z(03) VALUE ZERO.                             
098700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
098800                                                                          
098900   03  NOVA-DHL-ORIGIN.                                                   
099000     05  FILLER      PIC X(25) VALUE '!F T N 1740  700 L 2 1 3 '.         
099100     05  FILLER      PIC X(1)  VALUE '"'.                                 
099200     05  FILLER      PIC X(09) VALUE 'ORIGIN:  '.                         
099300     05  FILLER      PIC X(03) VALUE 'GOT'.                               
099400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
099500*3 SMALL LABELS AT BOTTOM OF DHL ETIKETT.                                 
099600*LABEL1                                                                   
099700   03  NOVA-DHL-31-BEGMTRAD1.                                             
099800     05  FILLER      PIC X(25) VALUE '!F T N 1840  120 L 2 1 4 '.         
099900     05  FILLER      PIC X(1)  VALUE '"'.                                 
100000     05  FILLER      PIC X(03) VALUE 'TO:'.                               
100100     05  NOVA-DHL-31-BEGMT-RAD1 PIC X(30) VALUE SPACE.                    
100200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
100300                                                                          
100400   03  NOVA-DHL-31-BEGMTRAD2.                                             
100500     05  FILLER      PIC X(25) VALUE '!F T N 1880  120 L 2 1 4 '.         
100600     05  FILLER      PIC X(1)  VALUE '"'.                                 
100700     05  NOVA-DHL-31-BEGMT-RAD2 PIC X(30) VALUE SPACE.                    
100800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
100900                                                                          
101000   03  NOVA-DHL-31-ADGMTLAND.                                             
101100     05  FILLER      PIC X(25) VALUE '!F T N 1920  120 L 2 1 4 '.         
101200     05  FILLER      PIC X(1)  VALUE '"'.                                 
101300     05  NOVA-DHL-31-ADGMT-LAND PIC X(30) VALUE SPACE.                    
101400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
101500                                                                          
101600   03  NOVA-DHL-31-DATE.                                                  
101700     05  FILLER      PIC X(25) VALUE '!F T N 1810  120 L 2 1 3 '.         
101800     05  FILLER      PIC X(1)  VALUE '"'.                                 
101900     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
102000     05  NOVA-DHL-31-SHIPDATE-AA PIC 9(02) VALUE ZERO.                    
102100     05  FILLER                PIC X(01)   VALUE '-'.                     
102200     05  NOVA-DHL-31-SHIPDATE-MM PIC 9(02) VALUE ZERO.                    
102300     05  FILLER                PIC X(01)   VALUE '-'.                     
102400     05  NOVA-DHL-31-SHIPDATE-DD PIC 9(02) VALUE ZERO.                    
102500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
102600                                                                          
102700   03  NOVA-DHL-31-AWB.                                                   
102800     05  FILLER      PIC X(25) VALUE '!F T N 1980 100 L 2 1 3 '.          
102900     05  FILLER      PIC X(1)  VALUE '"'.                                 
103000     05  FILLER      PIC X(05) VALUE 'AWB:'.                              
103100     05  NOVA-DHL-31-IDAWB PIC X(10) VALUE ZERO.                          
103200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
103300                                                                          
103400*LABEL2                                                                   
103500   03  NOVA-DHL-32-BEGMTRAD1.                                             
103600     05  FILLER      PIC X(25) VALUE '!F T N 1840  450 L 2 1 4 '.         
103700     05  FILLER      PIC X(1)  VALUE '"'.                                 
103800     05  FILLER                PIC X(03)  VALUE 'TO:'.                    
103900     05  NOVA-DHL-32-BEGMT-RAD1 PIC X(30) VALUE SPACE.                    
104000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
104100                                                                          
104200   03  NOVA-DHL-32-BEGMTRAD2.                                             
104300     05  FILLER      PIC X(25) VALUE '!F T N 1880  450 L 2 1 4 '.         
104400     05  FILLER      PIC X(1)  VALUE '"'.                                 
104500     05  NOVA-DHL-32-BEGMT-RAD2 PIC X(30) VALUE SPACE.                    
104600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
104700                                                                          
104800   03  NOVA-DHL-32-ADGMTLAND.                                             
104900     05  FILLER      PIC X(25) VALUE '!F T N 1920  450 L 2 1 4 '.         
105000     05  FILLER      PIC X(1)  VALUE '"'.                                 
105100     05  NOVA-DHL-32-ADGMT-LAND PIC X(30) VALUE SPACE.                    
105200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
105300                                                                          
105400   03  NOVA-DHL-32-DATE.                                                  
105500     05  FILLER      PIC X(25) VALUE '!F T N 1810  450 L 2 1 3 '.         
105600     05  FILLER      PIC X(1)  VALUE '"'.                                 
105700     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
105800     05  NOVA-DHL-32-SHIPDATE-AA PIC 9(02) VALUE ZERO.                    
105900     05  FILLER                PIC X(01)   VALUE '-'.                     
106000     05  NOVA-DHL-32-SHIPDATE-MM PIC 9(02) VALUE ZERO.                    
106100     05  FILLER                PIC X(01)   VALUE '-'.                     
106200     05  NOVA-DHL-32-SHIPDATE-DD PIC 9(02) VALUE ZERO.                    
106300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
106400                                                                          
106500   03  NOVA-DHL-32-AWB.                                                   
106600     05  FILLER      PIC X(25) VALUE '!F T N 1980  450 L 2 1 3 '.         
106700     05  FILLER      PIC X(1)  VALUE '"'.                                 
106800     05  FILLER      PIC X(05) VALUE 'AWB: '.                             
106900     05  NOVA-DHL-32-IDAWB PIC X(10) VALUE ZERO.                          
107000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
107100*LABEL3                                                                   
107200   03  NOVA-DHL-33-BEGMTRAD1.                                             
107300     05  FILLER      PIC X(25) VALUE '!F T N 1840  780 L 2 1 4 '.         
107400     05  FILLER      PIC X(1)  VALUE '"'.                                 
107500     05  FILLER                PIC X(03)  VALUE 'TO:'.                    
107600     05  NOVA-DHL-33-BEGMT-RAD1 PIC X(30) VALUE SPACE.                    
107700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
107800                                                                          
107900   03  NOVA-DHL-33-BEGMTRAD2.                                             
108000     05  FILLER      PIC X(25) VALUE '!F T N 1880  780 L 2 1 4 '.         
108100     05  FILLER      PIC X(1)  VALUE '"'.                                 
108200     05  NOVA-DHL-33-BEGMT-RAD2 PIC X(30) VALUE SPACE.                    
108300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
108400                                                                          
108500   03  NOVA-DHL-33-ADGMTLAND.                                             
108600     05  FILLER      PIC X(25) VALUE '!F T N 1920  780 L 2 1 4 '.         
108700     05  FILLER      PIC X(1)  VALUE '"'.                                 
108800     05  NOVA-DHL-33-ADGMT-LAND PIC X(30) VALUE SPACE.                    
108900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
109000                                                                          
109100   03  NOVA-DHL-33-DATE.                                                  
109200     05  FILLER      PIC X(25) VALUE '!F T N 1810  780 L 2 1 3 '.         
109300     05  FILLER      PIC X(1)  VALUE '"'.                                 
109400     05  FILLER                PIC X(05)   VALUE 'DATE:'.                 
109500     05  NOVA-DHL-33-SHIPDATE-AA PIC 9(02) VALUE ZERO.                    
109600     05  FILLER                PIC X(01)   VALUE '-'.                     
109700     05  NOVA-DHL-33-SHIPDATE-MM PIC 9(02) VALUE ZERO.                    
109800     05  FILLER                PIC X(01)   VALUE '-'.                     
109900     05  NOVA-DHL-33-SHIPDATE-DD PIC 9(02) VALUE ZERO.                    
110000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
110100                                                                          
110200   03  NOVA-DHL-33-AWB.                                                   
110300     05  FILLER      PIC X(25) VALUE '!F T N 1980  775 L 2 1 3 '.         
110400     05  FILLER      PIC X(1)  VALUE '"'.                                 
110500     05  FILLER      PIC X(05) VALUE 'AWB: '.                             
110600     05  NOVA-DHL-33-IDAWB PIC X(10) VALUE ZERO.                          
110700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
110800                                                                          
110900*END OF NOVA-DHL-LABEL ***NOVA***NOVA***NOVA***NOVA***NOVA******          
111000*NOVA                                                                     
111100*NOVA                                                                     
111200*NOVA                                                                     
111300                                                                          
111400 01  FELKODER.                                                            
111500     03  FEL-IDDISTR             PIC 9(4)    VALUE ZERO.                  
111600     03  FEL-IDPRODNR            PIC 9(7)    VALUE ZERO.                  
111700     03  FEL-IDKOLLI             PIC 9(5)    VALUE ZERO.                  
111800     03  FEL-IDTRANS             PIC X(4)    VALUE SPACE.                 
111900                                                                          
112000 01  RETURKODER.                                                          
112100     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
112200     EJECT                                                                
112300 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
112400                                                                          
112500*01  FILLER -COPY WWDIST83    -RED TEST-IDDISTR.                          
112600     EJECT                                                                
112700*01  -COPY WWDIST84                                                       
112800     EJECT                                                                
112900*01  FILLER -COPY W475WKNT                                                
113000     EJECT                                                                
113100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
113200 01  GENERELLA-SUBPROGRAM.                                                
113300     03 W006PRS1                 PIC X(8)    VALUE 'W006PRS1'.            
113400     03 W006PRT                  PIC X(8)    VALUE 'W006PRT '.            
113500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
113600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
113700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
113800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
113900*                                                                         
114000     EJECT                                                                
114100 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
114200*   -COPY W006PRT                                                         
114300*                                                                         
114400 01  FILLER                      PIC X(16)   VALUE  'WORKAREA'.           
114500*01  -COPY WORKAREA                                                       
114600     EJECT                                                                
114700*01  -COPY W006PRAR                                                       
114800     EJECT                                                                
114900*                                                                         
115000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
115100                                                                          
115200*01  MID -COPY W4I33901                                                   
115300     EJECT                                                                
115400 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
115500                                                                          
115600*01  -COPY WMSGAREA                                                       
115700     EJECT                                                                
115800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
115900*                                                                         
116000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
116100                                                                          
116200 01  NYCKLAR-TILL-DLI.                                                    
116300                                                                          
116400   03  W-WDGXKEY-4545-X.                                                  
116500     05  W-IDHTYP-4545        PIC X(4)    VALUE '4545'.                   
116600     05  FILLER               PIC X(26)   VALUE LOW-VALUE.                
116700                                                                          
116800   03  W-KY4546-X.                                                        
116900     05  W-IDPRODNR           PIC S9(7)   VALUE ZERO COMP-3.              
117000     05  W-IDKOLLI            PIC S9(5)   VALUE ZERO COMP-3.              
117100                                                                          
117200   03  W-WDGXKEY-4523-X.                                                  
117300     05  W-IDHTYP-4523       PIC X(4)     VALUE '4523'.                   
117400     05  FILLER              PIC X(26)    VALUE LOW-VALUE.                
117500                                                                          
117600   03  W-KDSEGKEY-X.                                                      
117700     05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                      
117800                                                                          
117900   03  W-WDGXKEY-4543-X.                                                  
118000     05  W-IDHTYP-4543       PIC X(4)     VALUE '4543'.                   
118100     05  FILLER              PIC X(26)    VALUE LOW-VALUE.                
118200                                                                          
118300   03  W-KY4544-MIN-X.                                                    
118400     05  W-IDLANDX2-MIN      PIC X(2)   VALUE SPACE.                      
118500     05  FILLER              PIC X(20)  VALUE LOW-VALUE.                  
118600                                                                          
118700   03  W-KY4544-MAX-X.                                                    
118800     05  W-IDLANDX2-MAX      PIC X(2)   VALUE SPACE.                      
118900     05  FILLER              PIC X(20)  VALUE HIGH-VALUE.                 
119000                                                                          
119100   03  W-ADPOSTNR-X.                                                      
119200     05  W-ADPOSTNR          PIC X(10)   VALUE SPACE.                     
119300                                                                          
119400   03  W-IDGMT-X.                                                         
119500     05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.               
119600     05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.               
119700                                                                          
119800   03  W-WDB101KY-X.                                                      
119900     05 W-IDPARTNR           PIC X(9)    VALUE SPACE.                     
120000     05 W-IDFTG              PIC 9(2)    VALUE ZERO.                      
120100                                                                          
120200   03  W-IDDC-B6-X.                                                       
120300     05  W-IDDC-B6               PIC X(2)    VALUE '11'.                  
120400     EJECT                                                                
120500*    --- STATUS-KOD FRÅN IMS                                              
120600 01  STATUS-WS                   PIC XX.                                  
120700     88  SEGMENT-FINNS                       VALUE '  '.                  
120800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
120900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
121000     SKIP2                                                                
121100 01  GODK-STATUSKODER.                                                    
121200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
121300     SKIP3                                                                
121400 01  SSA1                        PIC X(128).                              
121500 01  SSA2                        PIC X(128).                              
121600 01  SSA3                        PIC X(64).                               
121700     EJECT                                                                
121800                                                                          
121900*    --- IMS FUNKTIONSKODER                                               
122000*01  -COPY W0003                                                          
122100     EJECT                                                                
122200*    ---  DLI INPUT-OUTPUT AREA                                           
122300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4524'.         
122400     SKIP3                                                                
122500 01  DLI-IO-4524.                                                         
122600*    03  -COPY WDGX4524                                                   
122700     EJECT                                                                
122800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4544'.         
122900     SKIP3                                                                
123000 01  DLI-IO-4544.                                                         
123100*    03  -COPY WDGX4544                                                   
123200     EJECT                                                                
123300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4546'.         
123400     SKIP3                                                                
123500 01  DLI-IO-4546.                                                         
123600*    03  -COPY WDGX4546                                                   
123700     EJECT                                                                
123800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-B201'.         
123900 01  DLI-IO-B201.                                                         
124000*    03  -COPY WDB201                                                     
124100     EJECT                                                                
124200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-B101'.         
124300 01  DLI-IO-B101.                                                         
124400*    03  -COPY WDB101                                                     
124500     EJECT                                                                
124600 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
124700 01   DLI-IO-AREA-B601.                                                   
124800*     03  -COPY WDB601                                                    
124900     EJECT                                                                
125000 LINKAGE SECTION.                                                         
125100*01  -COPY W0009   -PRE MSG-                                              
125200                                                                          
125300*01  -COPY W0009   -PRE ALT-                                              
125400     EJECT                                                                
125500*01  -COPY W0008   -PRE 4523-                                             
125600     05  FILLER                  PIC X.                                   
125700     EJECT                                                                
125800*01  -COPY W0008   -PRE 4543-                                             
125900     05  FILLER                  PIC X.                                   
126000     EJECT                                                                
126100*01  -COPY W0008   -PRE 4545-                                             
126200     05  FILLER                  PIC X.                                   
126300     EJECT                                                                
126400*01  -COPY W0008   -PRE WDB2-                                             
126500     05  FILLER                  PIC X.                                   
126600     EJECT                                                                
126700*01  -COPY W0008   -PRE WDB1-                                             
126800     05  FILLER                  PIC X.                                   
126900     SKIP2                                                                
127000*01  -COPY W0008     -PRE WDB6-                                           
127100     05  FILLER                  PIC X.                                   
127200     EJECT                                                                
127300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
127400                           4543-PCB 4545-PCB 4523-PCB                     
127500                           WDB2-PCB WDB1-PCB WDB6-PCB.                    
127600 MAIN SECTION.                                                            
127700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
127800                           4543-PCB 4545-PCB 4523-PCB                     
127900                           WDB2-PCB WDB1-PCB WDB6-PCB.                    
128000                                                                          
128100     PERFORM IMS-GET-MSG                                                  
128200     IF SEGMENT-FINNS                                                     
128300       IF MSG-KDTRTYP = 'X'                                               
128400         PERFORM A-INIT                                                   
128500                                                                          
128600         PERFORM IMS-GHU-WDGX4546                                         
128700                                                                          
128800         PERFORM B-TA-FRAM-DATA                                           
128900                                                                          
129000***OLD: DHL FOR MARKPOINT PRINTER:                                        
129100***      PERFORM C-SKRIV-ETIKETTER                                        
129101***                                                                       
129110***NEW: DHL FOR NOVA 6 PRINTER:                                           
129200         PERFORM D-PRINT-NOVA-DHL-LABEL                                   
129300                                                                          
129400       END-IF                                                             
129500     END-IF                                                               
129600                                                                          
129700     MOVE ZERO TO RETURN-CODE                                             
129800     GOBACK                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 A-INIT SECTION.                                                          
130200                                                                          
130300     MOVE JA  TO ALLT-SW                                                  
130400     MOVE +1                              TO IX                           
130500                                                                          
130600     MOVE MSG-INDATA-MINUS-1-TRANSKOD     TO MID-W4I33901                 
130700                                                                          
130800     IF MID-IDDISTR NOT NUMERIC                                           
130900       MOVE MID-IDDISTR                   TO FEL-IDDISTR                  
131000       MOVE NEJ TO ALLT-SW                                                
131100     END-IF                                                               
131200                                                                          
131300     IF MID-IDPRODNR NUMERIC                                              
131400       MOVE MID-IDPRODNR                  TO W-IDPRODNR                   
131500     ELSE                                                                 
131600       MOVE MID-IDPRODNR                  TO FEL-IDPRODNR                 
131700       MOVE NEJ TO ALLT-SW                                                
131800     END-IF                                                               
131900                                                                          
132000     IF MID-IDKOLLI NUMERIC                                               
132100       MOVE MID-IDKOLLI                   TO W-IDKOLLI                    
132200     ELSE                                                                 
132300       MOVE MID-IDKOLLI                   TO FEL-IDKOLLI                  
132400       MOVE NEJ TO ALLT-SW                                                
132500     END-IF                                                               
132600                                                                          
132700     IF MSG-IDTRANS-1 NOT = '4333'                                        
132800        MOVE MSG-IDTRANS-1                TO FEL-IDTRANS                  
132900        MOVE NEJ TO ALLT-SW                                               
133000     END-IF                                                               
133100                                                                          
133200     IF NOT ALLT-OK                                                       
133300        MOVE '*** FEL PÅ INDATAT KOLLA PÅ FEL-  **'                       
133400                               TO FELTEXT                                 
133500        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
133600     END-IF                                                               
133700                                                                          
133800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
133900     .                                                                    
134000     EJECT                                                                
134100 B-TA-FRAM-DATA SECTION.                                                  
134200                                                                          
134300* KONTONR                                                                 
134400     IF 4546-IDKONTO = ZERO                                               
134500       MOVE MID-IDDISTR          TO WS-IDDISTR                            
134600                                                                          
134700       SET TAB-IX                TO +1                                    
134800       SEARCH KONTOTABELL                                                 
134900         AT END                                                           
135000           MOVE ZERO             TO WS-KONTONR-KLASS-0                    
135100           MOVE ZERO             TO WS-KONTONR-KLASS-1                    
135200         WHEN TAB-IDDISTR(TAB-IX) = WS-IDDISTR                            
135300           MOVE TAB-KONTONR-KLASS-0 (TAB-IX)                              
135400                                   TO WS-KONTONR-KLASS-0                  
135500           MOVE TAB-KONTONR-KLASS-1 (TAB-IX)                              
135600                                   TO WS-KONTONR-KLASS-1                  
135700       END-SEARCH                                                         
135800       IF 4546-KDORDKL = ZERO                                             
135900         MOVE WS-KONTONR-KLASS-0 TO 4546-IDKONTO                          
136000       ELSE                                                               
136100         MOVE WS-KONTONR-KLASS-1 TO 4546-IDKONTO                          
136200       END-IF                                                             
136300                                                                          
136400       MOVE JA TO REPL-SW                                                 
136500     END-IF                                                               
136600                                                                          
136700* DESTINATION                                                             
136800     IF 4546-IDCITY = SPACE                                               
136900       PERFORM BA-TA-FRAM-POSTNR                                          
137000       PERFORM BB-TA-FRAM-IDCITY                                          
137100       MOVE JA TO REPL-SW                                                 
137200*        CALL ABEND USING RKOD-ABEND-MED-DUMP                             
137300                                                                          
137400     END-IF                                                               
137500                                                                          
137600* IDAWB                                                                   
137700     IF 4546-IDAWB = ZERO                                                 
137800       PERFORM BC-TA-FRAM-IDAWB                                           
137900                                                                          
138000       MOVE WS-IDAWB-1-9       TO 4546-IDAWB                              
138100       MOVE WS-IDAWB-10        TO 4546-REKSIFFR-AWB                       
138200                                                                          
138300       MOVE JA TO REPL-SW                                                 
138400     END-IF                                                               
138500                                                                          
138600     IF REPL-OK                                                           
138700       PERFORM IMS-REPL-WDGX4546                                          
138800     END-IF                                                               
138900     .                                                                    
139000     EJECT                                                                
139100 BA-TA-FRAM-POSTNR SECTION.                                               
139200                                                                          
139300     MOVE SPACE                TO POSTNUMMER                              
139400     MOVE MID-IDDISTR          TO TEST-IDDISTR                            
139500     MOVE 4546-ADGMT-PADR      TO WRAD                                    
139600                                                                          
139700     PERFORM S01-LETA-I-WRAD                                              
139800                                                                          
139900     IF FUNNEN = JA                                                       
140000       MOVE 4546-ADGMT-PADR (POS1 + POS2 : ANTAL-SOLAR)                   
140100                               TO POSTNUMMER                              
140200       MOVE POSTNUMMER         TO WS-POSTNR-KOD                           
140300     END-IF                                                               
140400     .                                                                    
140500     EJECT                                                                
140600 BB-TA-FRAM-IDCITY SECTION.                                               
140700                                                                          
140800     MOVE MID-IDDISTR    TO W-IDDISTR                                     
140900                            TEST-IDDISTR                                  
141000     MOVE 4546-IDKUNDNR  TO W-IDKUNDNR                                    
141100                                                                          
141200     IF DIST83-DHL-SI OR                                                  
141300        DIST83-DHL-HU OR                                                  
141400        DIST83-DHL-CZ OR                                                  
141500        DIST83-DHL-SK OR                                                  
141600        DIST83-DHL-GB                                                     
141700       PERFORM BBA-TA-FRAM-LANDSKOD                                       
141800                                                                          
141900     ELSE                                                                 
142000                                                                          
142100       PERFORM IMS-GU-WDB601                                              
142200       MOVE DCS-IDFTG                    TO W-IDFTG                       
142300                                                                          
142400       PERFORM IMS-GU-WDB201                                              
142500                                                                          
142600       MOVE GMT-IDPARTNR                 TO W-IDPARTNR                    
142700       PERFORM IMS-GU-WDB101                                              
142800     END-IF                                                               
142900                                                                          
143000     MOVE BET-IDLANDX2   TO WS-POSTNR-ISO                                 
143100                            W-IDLANDX2-MIN                                
143200                            W-IDLANDX2-MAX                                
143300     MOVE WS-POSTNR-KOD  TO W-ADPOSTNR                                    
143400*                                                                         
143500*SPEC. MALTA - POSTNR.(4546) STÄMMER EJ MED POSTNR. PÅ HTYP 4544          
143600*            - EN ENDA IDCITY PÅ 4544, INTERVALL 0 - 999999999            
143700*                                                                         
143800     IF WS-POSTNR-ISO = 'MT'                                              
143900       MOVE ZERO         TO W-ADPOSTNR                                    
144000     END-IF                                                               
144100*                                                                         
144200*SPEC. IRLAND- POSTNR.(4546) STÄMMER EJ MED POSTNR. PÅ HTYP 4544          
144300*            - EN ENDA IDCITY PÅ 4544, INTERVALL 0 - 999999999            
144400*                                                                         
144500     IF WS-POSTNR-ISO = 'IE'                                              
144600       MOVE ZERO         TO W-ADPOSTNR                                    
144700     END-IF                                                               
144800*                                                                         
144900     PERFORM IMS-GU-WDGX4544                                              
145000                                                                          
145100     IF SEGMENT-FINNS                                                     
145200       MOVE 4544-IDCITY  TO 4546-IDCITY                                   
145300     ELSE                                                                 
145400       MOVE SPACE        TO 4546-IDCITY                                   
145500     END-IF                                                               
145600     .                                                                    
145700     EJECT                                                                
145800 BBA-TA-FRAM-LANDSKOD SECTION.                                            
145900                                                                          
146000     IF DIST83-DHL-SI                                                     
146100       MOVE 'SI'         TO BET-IDLANDX2                                  
146200     END-IF                                                               
146300                                                                          
146400     IF DIST83-DHL-HU                                                     
146500       MOVE 'HU'         TO BET-IDLANDX2                                  
146600     END-IF                                                               
146700                                                                          
146800     IF DIST83-DHL-CZ                                                     
146900       MOVE 'CZ'         TO BET-IDLANDX2                                  
147000     END-IF                                                               
147100                                                                          
147200     IF DIST83-DHL-SK                                                     
147300       MOVE 'SK'         TO BET-IDLANDX2                                  
147400     END-IF                                                               
147500                                                                          
147600     IF DIST83-DHL-GB                                                     
147700       MOVE 'GB'         TO BET-IDLANDX2                                  
147800     END-IF                                                               
147900     .                                                                    
148000     EJECT                                                                
148100 BC-TA-FRAM-IDAWB SECTION.                                                
148200                                                                          
148300     PERFORM IMS-GHU-WDGX4524                                             
148400                                                                          
148500     MOVE 4524-IDAWB-AKT TO WS-IDAWB-1-9                                  
148600     DIVIDE 7 INTO WS-IDAWB-1-9 GIVING WS-RESULTAT                        
148700                         REMAINDER WS-REST                                
148800                                                                          
148900     MOVE WS-REST TO WS-IDAWB-10                                          
149000                                                                          
149100     ADD +1        TO 4524-IDAWB-AKT                                      
149200     IF 4524-IDAWB-AKT > 4524-IDAWB-MAX                                   
149300       MOVE 4524-IDAWB-MIN TO 4524-IDAWB-AKT                              
149400     END-IF                                                               
149500                                                                          
149600     PERFORM IMS-REPL-WDGX4524                                            
149700     .                                                                    
149800     EJECT                                                                
149900 S01-LETA-I-WRAD SECTION.                                                 
150000                                                                          
150100     MOVE NEJ TO FUNNEN                                                   
150200                                                                          
150300     IF DIST83-DHL-GB                                                     
150400       INSPECT WRAD CONVERTING BLANDAT TO SOLAR-2                         
150500     ELSE                                                                 
150600       INSPECT WRAD CONVERTING SIFFROR TO SOLAR-1                         
150700     END-IF                                                               
150800     MOVE 1 TO POS1                                                       
150900                                                                          
151000     PERFORM UNTIL FUNNEN = JA OR POS1 > LENGTH OF WRAD                   
151100       MOVE ZERO TO POS2                                                  
151200                                                                          
151300       INSPECT WRAD (POS1:) TALLYING POS2                                 
151400               FOR CHARACTERS BEFORE INITIAL SOL                          
151500                                                                          
151600       IF POS2 < FUNCTION LENGTH (WRAD (POS1:))                           
151700         MOVE ZERO TO ANTAL-SOLAR                                         
151800                                                                          
151900         INSPECT WRAD (POS1 + POS2:) TALLYING ANTAL-SOLAR                 
152000                 FOR LEADING SOL                                          
152100                                                                          
152200         IF (DIST83-DHL-GB AND ANTAL-SOLAR >= 2 AND <= 4) OR              
152300            (ANTAL-SOLAR >= 3 AND <= 6)                                   
152400           MOVE JA TO FUNNEN                                              
152500                                                                          
152600         ELSE                                                             
152700           ADD POS2 ANTAL-SOLAR TO POS1                                   
152800         END-IF                                                           
152900                                                                          
153000       ELSE                                                               
153100         ADD POS2 TO POS1                                                 
153200       END-IF                                                             
153300     END-PERFORM                                                          
153400     .                                                                    
153500     EJECT                                                                
153600 C-SKRIV-ETIKETTER SECTION.                                               
153700                                                                          
153800     MOVE '4KF11DHL'                   TO LISTVAL                         
153900     CALL W006PRT USING PRT-W006PRT                                       
154000                                                                          
154100     MOVE +1                           TO IX                              
154200     MOVE NEJ                          TO POST-HITTAD-SW                  
154300                                                                          
154400     MOVE 4546-BEGMT-RAD1              TO DHL-BEGMT-RAD1                  
154500*    MOVE 4546-BEGMT-RAD1              TO DHL-BEGMT-RAD1-FROM             
154600     MOVE 4546-BEGMT-RAD1              TO DHL-TO-BEGMT-RAD1               
154700     MOVE 4546-BEGMT-RAD1              TO DHL-31-BEGMT-RAD1               
154800     MOVE 4546-BEGMT-RAD1              TO DHL-32-BEGMT-RAD1               
154900     MOVE 4546-BEGMT-RAD1              TO DHL-33-BEGMT-RAD1               
155000     MOVE 4546-BEGMT-RAD2              TO DHL-BEGMT-RAD2                  
155100*    MOVE 4546-BEGMT-RAD2              TO DHL-BEGMT-RAD1-FROM             
155200     MOVE 4546-BEGMT-RAD2              TO DHL-TO-BEGMT-RAD2               
155300     MOVE 4546-BEGMT-RAD2              TO DHL-31-BEGMT-RAD2               
155400     MOVE 4546-BEGMT-RAD2              TO DHL-32-BEGMT-RAD2               
155500     MOVE 4546-BEGMT-RAD2              TO DHL-33-BEGMT-RAD2               
155600     MOVE 4546-ADGMT-GATA              TO DHL-ADGMT-GATA                  
155700*    MOVE 4546-ADGMT-GATA              TO DHL-ADGMT-GATA-FROM             
155800     MOVE 4546-ADGMT-GATA              TO DHL-TO-ADGMT-GATA               
155900     MOVE 4546-ADGMT-PADR              TO DHL-ADGMT-PADR                  
156000*    MOVE 4546-ADGMT-PADR              TO DHL-ADGMT-PADR-FROM             
156100     MOVE 4546-ADGMT-PADR              TO DHL-TO-ADGMT-PADR               
156200     MOVE 4546-ADGMT-LAND              TO DHL-ADGMT-LAND                  
156300*    MOVE 4546-ADGMT-LAND              TO DHL-ADGMT-LAND-FROM             
156400     MOVE 4546-ADGMT-LAND              TO DHL-TO-ADGMT-LAND               
156500     MOVE 4546-ADGMT-LAND              TO DHL-31-ADGMT-LAND               
156600     MOVE 4546-ADGMT-LAND              TO DHL-32-ADGMT-LAND               
156700     MOVE 4546-ADGMT-LAND              TO DHL-33-ADGMT-LAND               
156800     MOVE 4546-IDKONTO                 TO DHL-ACCOUNTNR                   
156900     MOVE 4546-IDKUNDNR                TO DHL-IDKUNDNR                    
157000     MOVE 4546-IDORDNR5                TO DHL-IDORDNR5                    
157100     MOVE 1                            TO DHL-ANTAL                       
157200     MOVE 1                            TO DHL-TOTAL                       
157300     MOVE 4546-VKORDBTO                TO WS-VKORDBTO                     
157400     MOVE WS-KILO                      TO DHL-KILO                        
157500     MOVE WS-HEKTO                     TO DHL-HEKTO                       
157600     MOVE 4546-VLORDBTO                TO WS-VLORDBTO                     
157700     MOVE WS-VLM3                      TO DHL-VLM3                        
157800     MOVE WS-VLCM3                     TO DHL-VLCM3                       
157900     MOVE 4546-DIKOLLIL                TO DHL-DIKOLLIL                    
158000     MOVE 4546-DIKOLLIB                TO DHL-DIKOLLIB                    
158100     MOVE 4546-DIKOLLIH                TO DHL-DIKOLLIH                    
158200     MOVE ZERO                         TO DHL-VATNR                       
158300     MOVE ZERO                         TO DHL-TO-VATNR                    
158400     MOVE ZERO                         TO DHL-VALUE-0                     
158500     MOVE ZERO                         TO DHL-31-IDAWB                    
158600     MOVE ZERO                         TO DHL-32-IDAWB                    
158700     MOVE ZERO                         TO DHL-33-IDAWB                    
158800     MOVE ZERO                         TO DHL-IDAWB-SMALL-BARCODE         
158900     MOVE ZERO                         TO DHL-IDAWB-BIG-BARCODE           
159000     MOVE ZERO                         TO DHL-IDAWB-TEXT                  
159100     MOVE ZERO                         TO DHL-IDAWB                       
159200     MOVE 4546-IDCITY                  TO DHL-IDCITY                      
159300     MOVE 4546-IDCITY                  TO DHL-DEST-IDCITY                 
159400                                                                          
159500     MOVE 4546-IDAWB                   TO WS-IDAWB-1-9                    
159600     MOVE 4546-REKSIFFR-AWB            TO WS-IDAWB-10                     
159700                                                                          
159800     MOVE WS-IDAWB                     TO DHL-IDAWB                       
159900     INSPECT DHL-IDAWB REPLACING LEADING ZERO BY SPACE                    
160000     MOVE WS-IDAWB                     TO DHL-IDAWB-TEXT                  
160100     INSPECT DHL-IDAWB-TEXT REPLACING LEADING ZERO BY SPACE               
160200     MOVE WS-IDAWB                     TO DHL-IDAWB-SMALL-BARCODE         
160300     MOVE WS-IDAWB                     TO DHL-IDAWB-BIG-BARCODE           
160400     MOVE WS-IDAWB                     TO DHL-31-IDAWB                    
160500     INSPECT DHL-31-IDAWB   REPLACING LEADING ZERO BY SPACE               
160600     MOVE WS-IDAWB                     TO DHL-32-IDAWB                    
160700     INSPECT DHL-32-IDAWB   REPLACING LEADING ZERO BY SPACE               
160800     MOVE WS-IDAWB                     TO DHL-33-IDAWB                    
160900     INSPECT DHL-33-IDAWB   REPLACING LEADING ZERO BY SPACE               
161000     MOVE WS-DAGENS-DATUM (3:2)        TO DHL-SHIPDATE-AA                 
161100     MOVE WS-DAGENS-DATUM (5:2)        TO DHL-SHIPDATE-MM                 
161200     MOVE WS-DAGENS-DATUM (7:2)        TO DHL-SHIPDATE-DD                 
161300     MOVE WS-DAGENS-DATUM (3:2)        TO DHL-31-SHIPDATE-AA              
161400     MOVE WS-DAGENS-DATUM (5:2)        TO DHL-31-SHIPDATE-MM              
161500     MOVE WS-DAGENS-DATUM (7:2)        TO DHL-31-SHIPDATE-DD              
161600     MOVE WS-DAGENS-DATUM (3:2)        TO DHL-32-SHIPDATE-AA              
161700     MOVE WS-DAGENS-DATUM (5:2)        TO DHL-32-SHIPDATE-MM              
161800     MOVE WS-DAGENS-DATUM (7:2)        TO DHL-32-SHIPDATE-DD              
161900     MOVE WS-DAGENS-DATUM (3:2)        TO DHL-33-SHIPDATE-AA              
162000     MOVE WS-DAGENS-DATUM (5:2)        TO DHL-33-SHIPDATE-MM              
162100     MOVE WS-DAGENS-DATUM (7:2)        TO DHL-33-SHIPDATE-DD              
162200                                                                          
162300     MOVE SPACE       TO DHL-RAD                                          
162400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN LISTVAL                   
162500                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
162600                                                                          
162700     MOVE DHL-STYR-01 TO DHL-RAD                                          
162800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
162900                         ALT-PCB PRT-NYSIDA-RAD1 DHL-RAD                  
163000                                                                          
163100*DATA-FIELDS                                                              
163200*4546                                                                     
163300     MOVE DHL-HEAD-BEGMT-RAD1       TO DHL-RAD                            
163400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
163500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
163600                                                                          
163700     MOVE DHL-HEAD-BEGMT-RAD2       TO DHL-RAD                            
163800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
163900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
164000                                                                          
164100     MOVE DHL-HEAD-ADGMT-GATA       TO DHL-RAD                            
164200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
164300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
164400                                                                          
164500     MOVE DHL-HEAD-ADGMT-PADR       TO DHL-RAD                            
164600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
164700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
164800                                                                          
164900     MOVE DHL-HEAD-ADGMT-LAND       TO DHL-RAD                            
165000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
165100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
165200                                                                          
165300     MOVE DHL-HEAD-TELNR            TO DHL-RAD                            
165400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
165500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
165600                                                                          
165700     MOVE DHL-HEAD-FAXNR            TO DHL-RAD                            
165800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
165900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
166000                                                                          
166100     MOVE DHL-HEAD-AWB-BARCODE      TO DHL-RAD                            
166200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
166300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
166400                                                                          
166500     MOVE DHL-AWB-TEXT              TO DHL-RAD                            
166600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
166700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
166800*FROM                                                                     
166900     MOVE DHL-RUB-FROM            TO DHL-RAD                              
167000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
167100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
167200                                                                          
167300     MOVE DHL-FROM-BEGMTRAD1      TO DHL-RAD                              
167400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
167500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
167600                                                                          
167700     MOVE DHL-FROM-BEGMTRAD2      TO DHL-RAD                              
167800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
167900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
168000                                                                          
168100     MOVE DHL-FROM-ADGMTGATA      TO DHL-RAD                              
168200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
168300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
168400                                                                          
168500     MOVE DHL-FROM-ADGMTPADR      TO DHL-RAD                              
168600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
168700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
168800                                                                          
168900     MOVE DHL-FROM-ADGMTLAND      TO DHL-RAD                              
169000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
169100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
169200                                                                          
169300     MOVE DHL-FROM-TELNR          TO DHL-RAD                              
169400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
169500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
169600                                                                          
169700     MOVE DHL-FROM-FAXNR          TO DHL-RAD                              
169800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
169900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
170000                                                                          
170100     MOVE DHL-FROM-VAT-SPACE      TO DHL-RAD                              
170200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
170300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
170400                                                                          
170500     MOVE DHL-FROM-ACCOUNTNR      TO DHL-RAD                              
170600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
170700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
170800                                                                          
170900     MOVE DHL-FROM-REFNR          TO DHL-RAD                              
171000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
171100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
171200                                                                          
171300     MOVE DHL-FROM-IDKUND-IDORD   TO DHL-RAD                              
171400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
171500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
171600                                                                          
171700     MOVE DHL-FROM-SHIP-DATE      TO DHL-RAD                              
171800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
171900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
172000*TO/TILL                                                                  
172100     MOVE DHL-RUB-TO                TO DHL-RAD                            
172200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
172300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
172400                                                                          
172500     MOVE DHL-TO-BEGMTRAD1         TO DHL-RAD                             
172600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
172700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
172800                                                                          
172900     MOVE DHL-TO-BEGMTRAD2         TO DHL-RAD                             
173000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
173100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
173200                                                                          
173300     MOVE DHL-TO-ADGMTGATA         TO DHL-RAD                             
173400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
173500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
173600                                                                          
173700     MOVE DHL-TO-ADGMTPADR         TO DHL-RAD                             
173800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
173900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
174000                                                                          
174100     MOVE DHL-TO-ADGMTLAND         TO DHL-RAD                             
174200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
174300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
174400                                                                          
174500     MOVE DHL-TO-TELNR              TO DHL-RAD                            
174600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
174700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
174800                                                                          
174900     MOVE DHL-TO-FAXNR              TO DHL-RAD                            
175000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
175100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
175200     MOVE DHL-TO-VAT-SPACE          TO DHL-RAD                            
175300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
175400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
175500*IMPORT/EXPORT                                                            
175600     MOVE DHL-IMPORT-EXPORT-TYPE    TO DHL-RAD                            
175700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
175800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
175900                                                                          
176000     MOVE DHL-VALUE                 TO DHL-RAD                            
176100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
176200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
176300                                                                          
176400     MOVE DHL-WEIGHT                TO DHL-RAD                            
176500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
176600                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
176700                                                                          
176800     MOVE DHL-DIM-WEIGHT            TO DHL-RAD                            
176900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
177000                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
177100                                                                          
177200     MOVE DHL-DIMENSIONS-TEXT       TO DHL-RAD                            
177300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
177400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
177500                                                                          
177600     MOVE DHL-DIMENSIONS            TO DHL-RAD                            
177700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
177800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
177900*DESCRIPTIONS                                                             
178000     MOVE DHL-DESCRIPTION           TO DHL-RAD                            
178100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
178200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
178300                                                                          
178400     MOVE DHL-SERVICE               TO DHL-RAD                            
178500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
178600                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
178700*ACCORDING TO WARSAW CONVENTION AND DHL TERMS & CONDITIONS.               
178800     MOVE DHL-DHL-TERMS1            TO DHL-RAD                            
178900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
179000                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
179100                                                                          
179200     MOVE DHL-DHL-TERMS2            TO DHL-RAD                            
179300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
179400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
179500                                                                          
179600     MOVE DHL-DHL-TERMS3            TO DHL-RAD                            
179700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
179800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
179900                                                                          
180000     MOVE DHL-DHL-TERMS4            TO DHL-RAD                            
180100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
180200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
180300*PRODUCT ECX                                                              
180400     MOVE DHL-PRODUCT               TO DHL-RAD                            
180500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
180600                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
180700                                                                          
180800     MOVE DHL-LINE-TOP              TO DHL-RAD                            
180900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
181000                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
181100                                                                          
181200     MOVE DHL-LINE-LEFT             TO DHL-RAD                            
181300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
181400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
181500                                                                          
181600     MOVE DHL-LINE-RIGHT            TO DHL-RAD                            
181700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
181800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
181900                                                                          
182000     MOVE DHL-LINE-BOTTOM           TO DHL-RAD                            
182100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
182200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
182300                                                                          
182400     MOVE DHL-ECX                   TO DHL-RAD                            
182500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
182600                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
182700*DESTINATION                                                              
182800     MOVE DHL-DESTINATION           TO DHL-RAD                            
182900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
183000                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
183100                                                                          
183200     MOVE DHL-CITY                  TO DHL-RAD                            
183300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
183400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
183500                                                                          
183600     MOVE DHL-DESTINATION-BARCODE   TO DHL-RAD                            
183700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
183800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
183900*AWB                                                                      
184000     MOVE DHL-AWB                   TO DHL-RAD                            
184100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
184200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
184300                                                                          
184400     MOVE DHL-AWB-BARCODE           TO DHL-RAD                            
184500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
184600                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
184700                                                                          
184800     MOVE DHL-TXT-NON-NEGOTIABLE    TO DHL-RAD                            
184900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
185000                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
185100                                                                          
185200     MOVE DHL-WEB-TEXT              TO DHL-RAD                            
185300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
185400                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
185500*IDKOLLI-ORIGIN                                                           
185600     MOVE DHL-KOLLI                 TO DHL-RAD                            
185700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
185800                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
185900                                                                          
186000     MOVE DHL-ORIGIN                TO DHL-RAD                            
186100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
186200                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
186300*3 SMALL LABEL STICKERS AT BOTTOM                                         
186400*LABEL1                                                                   
186500     MOVE DHL-31-DATE               TO DHL-RAD                            
186600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
186700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
186800                                                                          
186900     MOVE DHL-31-BEGMTRAD1          TO DHL-RAD                            
187000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
187100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
187200                                                                          
187300     MOVE DHL-31-BEGMTRAD2          TO DHL-RAD                            
187400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
187500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
187600                                                                          
187700     MOVE DHL-31-ADGMTLAND          TO DHL-RAD                            
187800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
187900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
188000                                                                          
188100     MOVE DHL-31-AWB                TO DHL-RAD                            
188200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
188300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
188400*LABEL2                                                                   
188500     MOVE DHL-32-DATE               TO DHL-RAD                            
188600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
188700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
188800                                                                          
188900     MOVE DHL-32-BEGMTRAD1          TO DHL-RAD                            
189000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
189100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
189200                                                                          
189300     MOVE DHL-32-BEGMTRAD2          TO DHL-RAD                            
189400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
189500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
189600                                                                          
189700     MOVE DHL-32-ADGMTLAND          TO DHL-RAD                            
189800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
189900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
190000                                                                          
190100     MOVE DHL-32-AWB                TO DHL-RAD                            
190200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
190300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
190400*LABEL3                                                                   
190500     MOVE DHL-33-DATE               TO DHL-RAD                            
190600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
190700                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
190800                                                                          
190900     MOVE DHL-33-BEGMTRAD1          TO DHL-RAD                            
191000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
191100                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
191200                                                                          
191300     MOVE DHL-33-BEGMTRAD2          TO DHL-RAD                            
191400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
191500                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
191600                                                                          
191700     MOVE DHL-33-ADGMTLAND          TO DHL-RAD                            
191800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
191900                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
192000                                                                          
192100     MOVE DHL-33-AWB             TO DHL-RAD                               
192200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
192300                         ALT-PCB PRT-AFTER-1 DHL-RAD                      
192400                                                                          
192500     MOVE DHL-STYR-91 TO DHL-RAD                                          
192600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
192700                         ALT-PCB PRT-NYSIDA-RAD1 DHL-RAD                  
192800                                                                          
192900*CLOSE PRINTER                                                            
193000     MOVE SPACE       TO DHL-RAD                                          
193100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-CLOSE LISTVAL                  
193200                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
193300     .                                                                    
193400     EJECT                                                                
193500 D-PRINT-NOVA-DHL-LABEL  SECTION.                                         
193600                                                                          
193700     MOVE '4KF11DHL'                   TO LISTVAL                         
193800     CALL W006PRT USING PRT-W006PRT                                       
193900                                                                          
194000     MOVE +1                           TO IX                              
194100     MOVE NEJ                          TO POST-HITTAD-SW                  
194200                                                                          
194300     MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-BEGMT-RAD1             
194400*    MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-BEGMT-RAD1-FROM        
194500     MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-TO-BEGMT-RAD1          
194600     MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-31-BEGMT-RAD1          
194700     MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-32-BEGMT-RAD1          
194800     MOVE 4546-BEGMT-RAD1              TO NOVA-DHL-33-BEGMT-RAD1          
194900     MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-BEGMT-RAD2             
195000*    MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-BEGMT-RAD1-FROM        
195100     MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-TO-BEGMT-RAD2          
195200     MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-31-BEGMT-RAD2          
195300     MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-32-BEGMT-RAD2          
195400     MOVE 4546-BEGMT-RAD2              TO NOVA-DHL-33-BEGMT-RAD2          
195500     MOVE 4546-ADGMT-GATA              TO NOVA-DHL-ADGMT-GATA             
195600*    MOVE 4546-ADGMT-GATA              TO NOVA-DHL-ADGMT-GATA-FROM        
195700     MOVE 4546-ADGMT-GATA              TO NOVA-DHL-TO-ADGMT-GATA          
195800     MOVE 4546-ADGMT-PADR              TO NOVA-DHL-ADGMT-PADR             
195900*    MOVE 4546-ADGMT-PADR              TO NOVA-DHL-ADGMT-PADR-FROM        
196000     MOVE 4546-ADGMT-PADR              TO NOVA-DHL-TO-ADGMT-PADR          
196100     MOVE 4546-ADGMT-LAND              TO NOVA-DHL-ADGMT-LAND             
196200*    MOVE 4546-ADGMT-LAND              TO NOVA-DHL-ADGMT-LAND-FROM        
196300     MOVE 4546-ADGMT-LAND              TO NOVA-DHL-TO-ADGMT-LAND          
196400     MOVE 4546-ADGMT-LAND              TO NOVA-DHL-31-ADGMT-LAND          
196500     MOVE 4546-ADGMT-LAND              TO NOVA-DHL-32-ADGMT-LAND          
196600     MOVE 4546-ADGMT-LAND              TO NOVA-DHL-33-ADGMT-LAND          
196700     MOVE 4546-IDKONTO                 TO NOVA-DHL-ACCOUNTNR              
196800     MOVE 4546-IDKUNDNR                TO NOVA-DHL-IDKUNDNR               
196900     MOVE 4546-IDORDNR5                TO NOVA-DHL-IDORDNR5               
197000     MOVE 1                            TO NOVA-DHL-ANTAL                  
197100     MOVE 1                            TO NOVA-DHL-TOTAL                  
197200     MOVE 4546-VKORDBTO                TO WS-VKORDBTO                     
197300     MOVE WS-KILO                      TO NOVA-DHL-KILO                   
197400     MOVE WS-HEKTO                     TO NOVA-DHL-HEKTO                  
197500     MOVE 4546-VLORDBTO                TO WS-VLORDBTO                     
197600     MOVE WS-VLM3                      TO NOVA-DHL-VLM3                   
197700     MOVE WS-VLCM3                     TO NOVA-DHL-VLCM3                  
197800     MOVE 4546-DIKOLLIL                TO NOVA-DHL-DIKOLLIL               
197900     MOVE 4546-DIKOLLIB                TO NOVA-DHL-DIKOLLIB               
198000     MOVE 4546-DIKOLLIH                TO NOVA-DHL-DIKOLLIH               
198100     MOVE ZERO                         TO NOVA-DHL-VATNR                  
198200     MOVE ZERO                         TO NOVA-DHL-TO-VATNR               
198300     MOVE ZERO                         TO NOVA-DHL-VALUE-0                
198400     MOVE ZERO                         TO NOVA-DHL-31-IDAWB               
198500     MOVE ZERO                         TO NOVA-DHL-32-IDAWB               
198600     MOVE ZERO                         TO NOVA-DHL-33-IDAWB               
198700     MOVE ZERO                  TO NOVA-DHL-IDAWB-SMALL-BARCODE           
198800     MOVE ZERO                  TO NOVA-DHL-IDAWB-BIG-BARCODE             
198900     MOVE ZERO                         TO NOVA-DHL-IDAWB-TEXT             
199000     MOVE ZERO                         TO NOVA-DHL-IDAWB                  
199100     MOVE 4546-IDCITY                  TO NOVA-DHL-IDCITY                 
199200     MOVE 4546-IDCITY                  TO NOVA-DHL-DEST-IDCITY            
199300                                                                          
199400     MOVE 4546-IDAWB                   TO WS-IDAWB-1-9                    
199500     MOVE 4546-REKSIFFR-AWB            TO WS-IDAWB-10                     
199600                                                                          
199700     MOVE WS-IDAWB                     TO NOVA-DHL-IDAWB                  
199800     INSPECT NOVA-DHL-IDAWB REPLACING LEADING ZERO BY SPACE               
199900     MOVE WS-IDAWB                     TO NOVA-DHL-IDAWB-TEXT             
200000     INSPECT NOVA-DHL-IDAWB-TEXT REPLACING LEADING ZERO BY SPACE          
200100     MOVE WS-IDAWB                TO NOVA-DHL-IDAWB-SMALL-BARCODE         
200200     MOVE WS-IDAWB                TO NOVA-DHL-IDAWB-BIG-BARCODE           
200300     MOVE WS-IDAWB                     TO NOVA-DHL-31-IDAWB               
200400     INSPECT NOVA-DHL-31-IDAWB REPLACING LEADING ZERO BY SPACE            
200500     MOVE WS-IDAWB                     TO NOVA-DHL-32-IDAWB               
200600     INSPECT NOVA-DHL-32-IDAWB REPLACING LEADING ZERO BY SPACE            
200700     MOVE WS-IDAWB                     TO NOVA-DHL-33-IDAWB               
200800     INSPECT NOVA-DHL-33-IDAWB REPLACING LEADING ZERO BY SPACE            
200900     MOVE WS-DAGENS-DATUM (3:2)        TO NOVA-DHL-SHIPDATE-AA            
201000     MOVE WS-DAGENS-DATUM (5:2)        TO NOVA-DHL-SHIPDATE-MM            
201100     MOVE WS-DAGENS-DATUM (7:2)        TO NOVA-DHL-SHIPDATE-DD            
201200     MOVE WS-DAGENS-DATUM (3:2)        TO NOVA-DHL-31-SHIPDATE-AA         
201300     MOVE WS-DAGENS-DATUM (5:2)        TO NOVA-DHL-31-SHIPDATE-MM         
201400     MOVE WS-DAGENS-DATUM (7:2)        TO NOVA-DHL-31-SHIPDATE-DD         
201500     MOVE WS-DAGENS-DATUM (3:2)        TO NOVA-DHL-32-SHIPDATE-AA         
201600     MOVE WS-DAGENS-DATUM (5:2)        TO NOVA-DHL-32-SHIPDATE-MM         
201700     MOVE WS-DAGENS-DATUM (7:2)        TO NOVA-DHL-32-SHIPDATE-DD         
201800     MOVE WS-DAGENS-DATUM (3:2)        TO NOVA-DHL-33-SHIPDATE-AA         
201900     MOVE WS-DAGENS-DATUM (5:2)        TO NOVA-DHL-33-SHIPDATE-MM         
202000     MOVE WS-DAGENS-DATUM (7:2)        TO NOVA-DHL-33-SHIPDATE-DD         
202100                                                                          
202200     MOVE SPACE       TO NOVA-DHL-RAD                                     
202300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN LISTVAL                   
202400                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
202500                                                                          
202600     MOVE NOVA-DHL-STYR-01 TO NOVA-DHL-RAD                                
202700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
202800                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-DHL-RAD             
202900                                                                          
203000     MOVE DHL-STYR-42 TO NOVA-DHL-RAD                                     
203100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
203200                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-DHL-RAD             
203300                                                                          
203400*DATA-FIELDS                                                              
203500*4546                                                                     
203600     MOVE NOVA-DHL-HEAD-BEGMT-RAD1  TO NOVA-DHL-RAD                       
203700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
203800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
203900                                                                          
204000     MOVE NOVA-DHL-HEAD-BEGMT-RAD2  TO NOVA-DHL-RAD                       
204100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
204200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
204300                                                                          
204400     MOVE NOVA-DHL-HEAD-ADGMT-GATA  TO NOVA-DHL-RAD                       
204500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
204600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
204700                                                                          
204800     MOVE NOVA-DHL-HEAD-ADGMT-PADR  TO NOVA-DHL-RAD                       
204900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
205000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
205100                                                                          
205200     MOVE NOVA-DHL-HEAD-ADGMT-LAND  TO NOVA-DHL-RAD                       
205300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
205400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
205500                                                                          
205600     MOVE NOVA-DHL-HEAD-TELNR       TO NOVA-DHL-RAD                       
205700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
205800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
205900                                                                          
206000     MOVE NOVA-DHL-HEAD-FAXNR       TO NOVA-DHL-RAD                       
206100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
206200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
206300                                                                          
206400     MOVE NOVA-DHL-HEAD-AWB-BARCODE TO NOVA-DHL-RAD                       
206500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
206600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
206700                                                                          
206800     MOVE NOVA-DHL-AWB-TEXT         TO NOVA-DHL-RAD                       
206900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
207000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
207100*FROM                                                                     
207200     MOVE NOVA-DHL-RUB-FROM       TO NOVA-DHL-RAD                         
207300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
207400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
207500                                                                          
207600     MOVE NOVA-DHL-FROM-BEGMTRAD1 TO NOVA-DHL-RAD                         
207700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
207800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
207900                                                                          
208000     MOVE NOVA-DHL-FROM-BEGMTRAD2 TO NOVA-DHL-RAD                         
208100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
208200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
208300                                                                          
208400     MOVE NOVA-DHL-FROM-ADGMTGATA TO NOVA-DHL-RAD                         
208500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
208600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
208700                                                                          
208800     MOVE NOVA-DHL-FROM-ADGMTPADR TO NOVA-DHL-RAD                         
208900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
209000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
209100                                                                          
209200     MOVE NOVA-DHL-FROM-ADGMTLAND TO NOVA-DHL-RAD                         
209300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
209400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
209500                                                                          
209600     MOVE NOVA-DHL-FROM-TELNR     TO NOVA-DHL-RAD                         
209700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
209800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
209900                                                                          
210000     MOVE NOVA-DHL-FROM-FAXNR     TO NOVA-DHL-RAD                         
210100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
210200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
210300                                                                          
210400     MOVE NOVA-DHL-FROM-VAT-SPACE TO NOVA-DHL-RAD                         
210500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
210600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
210700                                                                          
210800     MOVE NOVA-DHL-FROM-ACCOUNTNR TO NOVA-DHL-RAD                         
210900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
211000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
211100                                                                          
211200     MOVE NOVA-DHL-FROM-REFNR     TO NOVA-DHL-RAD                         
211300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
211400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
211500                                                                          
211600     MOVE NOVA-DHL-FROM-IDKUND-IDORD TO NOVA-DHL-RAD                      
211700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
211800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
211900                                                                          
212000     MOVE NOVA-DHL-FROM-SHIP-DATE TO NOVA-DHL-RAD                         
212100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
212200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
212300*TO/TILL                                                                  
212400     MOVE NOVA-DHL-RUB-TO           TO NOVA-DHL-RAD                       
212500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
212600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
212700                                                                          
212800     MOVE NOVA-DHL-TO-BEGMTRAD1    TO NOVA-DHL-RAD                        
212900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
213000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
213100                                                                          
213200     MOVE NOVA-DHL-TO-BEGMTRAD2    TO NOVA-DHL-RAD                        
213300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
213400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
213500                                                                          
213600     MOVE NOVA-DHL-TO-ADGMTGATA    TO NOVA-DHL-RAD                        
213700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
213800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
213900                                                                          
214000     MOVE NOVA-DHL-TO-ADGMTPADR    TO NOVA-DHL-RAD                        
214100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
214200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
214300                                                                          
214400     MOVE NOVA-DHL-TO-ADGMTLAND    TO NOVA-DHL-RAD                        
214500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
214600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
214700                                                                          
214800     MOVE NOVA-DHL-TO-TELNR         TO NOVA-DHL-RAD                       
214900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
215000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
215100                                                                          
215200     MOVE NOVA-DHL-TO-FAXNR         TO NOVA-DHL-RAD                       
215300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
215400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
215500     MOVE NOVA-DHL-TO-VAT-SPACE     TO NOVA-DHL-RAD                       
215600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
215700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
215800*IMPORT/EXPORT                                                            
215900     MOVE NOVA-DHL-IMPORT-EXPORT-TYPE TO NOVA-DHL-RAD                     
216000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
216100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
216200                                                                          
216300     MOVE NOVA-DHL-VALUE            TO NOVA-DHL-RAD                       
216400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
216500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
216600                                                                          
216700     MOVE NOVA-DHL-WEIGHT           TO NOVA-DHL-RAD                       
216800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
216900                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
217000                                                                          
217100     MOVE NOVA-DHL-DIM-WEIGHT       TO NOVA-DHL-RAD                       
217200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
217300                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
217400                                                                          
217500     MOVE NOVA-DHL-DIMENSIONS-TEXT  TO NOVA-DHL-RAD                       
217600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
217700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
217800                                                                          
217900     MOVE NOVA-DHL-DIMENSIONS       TO NOVA-DHL-RAD                       
218000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
218100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
218200*DESCRIPTIONS                                                             
218300     MOVE NOVA-DHL-DESCRIPTION      TO NOVA-DHL-RAD                       
218400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
218500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
218600                                                                          
218700     MOVE NOVA-DHL-SERVICE          TO NOVA-DHL-RAD                       
218800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
218900                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
219000*ACCORDING TO WARSAW CONVENTION AND DHL TERMS & CONDITIONS.               
219100     MOVE NOVA-DHL-DHL-TERMS1  TO NOVA-DHL-RAD                            
219200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
219300                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
219400                                                                          
219500     MOVE NOVA-DHL-DHL-TERMS2       TO NOVA-DHL-RAD                       
219600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
219700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
219800                                                                          
219900     MOVE NOVA-DHL-DHL-TERMS3       TO NOVA-DHL-RAD                       
220000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
220100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
220200                                                                          
220300     MOVE NOVA-DHL-DHL-TERMS4       TO NOVA-DHL-RAD                       
220400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
220500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
220600*PRODUCT ECX                                                              
220700     MOVE NOVA-DHL-PRODUCT          TO NOVA-DHL-RAD                       
220800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
220900                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
221000                                                                          
221100     MOVE NOVA-DHL-LINE-TOP         TO NOVA-DHL-RAD                       
221200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
221300                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
221400                                                                          
221500     MOVE NOVA-DHL-LINE-LEFT        TO NOVA-DHL-RAD                       
221600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
221700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
221800                                                                          
221900     MOVE NOVA-DHL-LINE-RIGHT       TO NOVA-DHL-RAD                       
222000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
222100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
222200                                                                          
222300     MOVE NOVA-DHL-LINE-BOTTOM      TO NOVA-DHL-RAD                       
222400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
222500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
222600                                                                          
222700     MOVE NOVA-DHL-ECX              TO NOVA-DHL-RAD                       
222800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
222900                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
223000*DESTINATION                                                              
223100     MOVE NOVA-DHL-DESTINATION      TO NOVA-DHL-RAD                       
223200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
223300                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
223400                                                                          
223500     MOVE NOVA-DHL-CITY             TO NOVA-DHL-RAD                       
223600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
223700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
223800                                                                          
223900     MOVE NOVA-DHL-DESTINATION-BARCODE TO NOVA-DHL-RAD                    
224000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
224100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
224200*AWB                                                                      
224300     MOVE NOVA-DHL-AWB              TO NOVA-DHL-RAD                       
224400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
224500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
224600                                                                          
224700     MOVE NOVA-DHL-AWB-BARCODE      TO NOVA-DHL-RAD                       
224800     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
224900                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
225000                                                                          
225100     MOVE NOVA-DHL-TXT-NON-NEGOTIABLE TO NOVA-DHL-RAD                     
225200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
225300                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
225400                                                                          
225500     MOVE NOVA-DHL-WEB-TEXT         TO NOVA-DHL-RAD                       
225600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
225700                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
225800*IDKOLLI-ORIGIN                                                           
225900     MOVE NOVA-DHL-KOLLI            TO NOVA-DHL-RAD                       
226000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
226100                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
226200                                                                          
226300     MOVE NOVA-DHL-ORIGIN           TO NOVA-DHL-RAD                       
226400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
226500                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
226600*3 SMALL LABEL STICKERS AT BOTTOM                                         
226700*LABEL1                                                                   
226800     MOVE NOVA-DHL-31-DATE          TO NOVA-DHL-RAD                       
226900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
227000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
227100                                                                          
227200     MOVE NOVA-DHL-31-BEGMTRAD1     TO NOVA-DHL-RAD                       
227300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
227400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
227500                                                                          
227600     MOVE NOVA-DHL-31-BEGMTRAD2     TO NOVA-DHL-RAD                       
227700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
227800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
227900                                                                          
228000     MOVE NOVA-DHL-31-ADGMTLAND     TO NOVA-DHL-RAD                       
228100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
228200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
228300                                                                          
228400     MOVE NOVA-DHL-31-AWB           TO NOVA-DHL-RAD                       
228500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
228600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
228700*LABEL2                                                                   
228800     MOVE NOVA-DHL-32-DATE          TO NOVA-DHL-RAD                       
228900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
229000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
229100                                                                          
229200     MOVE NOVA-DHL-32-BEGMTRAD1     TO NOVA-DHL-RAD                       
229300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
229400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
229500                                                                          
229600     MOVE NOVA-DHL-32-BEGMTRAD2     TO NOVA-DHL-RAD                       
229700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
229800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
229900                                                                          
230000     MOVE NOVA-DHL-32-ADGMTLAND     TO NOVA-DHL-RAD                       
230100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
230200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
230300                                                                          
230400     MOVE NOVA-DHL-32-AWB           TO NOVA-DHL-RAD                       
230500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
230600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
230700*LABEL3                                                                   
230800     MOVE NOVA-DHL-33-DATE          TO NOVA-DHL-RAD                       
230900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
231000                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
231100                                                                          
231200     MOVE NOVA-DHL-33-BEGMTRAD1     TO NOVA-DHL-RAD                       
231300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
231400                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
231500                                                                          
231600     MOVE NOVA-DHL-33-BEGMTRAD2     TO NOVA-DHL-RAD                       
231700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
231800                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
231900                                                                          
232000     MOVE NOVA-DHL-33-ADGMTLAND     TO NOVA-DHL-RAD                       
232100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
232200                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
232300                                                                          
232400     MOVE NOVA-DHL-33-AWB        TO NOVA-DHL-RAD                          
232500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
232600                         ALT-PCB PRT-AFTER-1 NOVA-DHL-RAD                 
232700                                                                          
232800     MOVE NOVA-DHL-STYR-91 TO NOVA-DHL-RAD                                
232900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
233000                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-DHL-RAD             
233100                                                                          
233200*CLOSE PRINTER                                                            
233300     MOVE SPACE       TO NOVA-DHL-RAD                                     
233400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-CLOSE LISTVAL                  
233500                         ALT-PCB DUMMY-AREA DUMMY-AREA                    
233600     .                                                                    
233700     EJECT                                                                
233800* --- IMS SEKTIONER ---                                                   
233900                                                                          
234000 IMS-GET-MSG SECTION.                                                     
234100                                                                          
234200     MOVE '  QC' TO GODK-STATUSKODER                                      
234300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
234400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700     SKIP3                                                                
234800 IMS-GU-WDGX4544 SECTION.                                                 
234900                                                                          
235000     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4543-X ')'                    
235100          DELIMITED BY SIZE INTO SSA1                                     
235200     STRING 'WDGX4544(KY4544  =>' W-KY4544-MIN-X                          
235300                    '&KY4544  <=' W-KY4544-MAX-X                          
235400                    '&ADPOSTNF<=' W-ADPOSTNR-X                            
235500                    '&ADPOSTNT>=' W-ADPOSTNR-X ')'                        
235600          DELIMITED BY SIZE INTO SSA2                                     
235700     MOVE '  GE' TO GODK-STATUSKODER                                      
235800     CALL CBLTDLI USING GU 4543-PCB DLI-IO-4544 SSA1 SSA2                 
235900     MOVE 4543-STATUS-CODE TO STATUS-WS                                   
236000     PERFORM IMS-STATUSKONTROLL                                           
236100     .                                                                    
236200     EJECT                                                                
236300 IMS-GHU-WDGX4546 SECTION.                                                
236400                                                                          
236500     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4545-X ')'                    
236600          DELIMITED BY SIZE INTO SSA1                                     
236700     STRING 'WDGX4546(KY4546   =' W-KY4546-X ')'                          
236800          DELIMITED BY SIZE INTO SSA2                                     
236900     MOVE '  ' TO GODK-STATUSKODER                                        
237000     CALL CBLTDLI USING GHU 4545-PCB DLI-IO-4546 SSA1 SSA2                
237100     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
237200     PERFORM IMS-STATUSKONTROLL                                           
237300     .                                                                    
237400     SKIP2                                                                
237500 IMS-REPL-WDGX4546 SECTION.                                               
237600     MOVE '    ' TO GODK-STATUSKODER                                      
237700     CALL CBLTDLI USING REPL 4545-PCB DLI-IO-4546                         
237800     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
237900     PERFORM IMS-STATUSKONTROLL                                           
238000     SKIP2                                                                
238100     .                                                                    
238200 IMS-GHU-WDGX4524 SECTION.                                                
238300                                                                          
238400     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4523-X ')'                    
238500          DELIMITED BY SIZE INTO SSA1                                     
238600     MOVE 'WDGX4524' TO SSA2                                              
238700     MOVE '  ' TO GODK-STATUSKODER                                        
238800     CALL CBLTDLI USING GHU 4523-PCB DLI-IO-4524 SSA1 SSA2                
238900     MOVE 4523-STATUS-CODE TO STATUS-WS                                   
239000     PERFORM IMS-STATUSKONTROLL                                           
239100     .                                                                    
239200     SKIP2                                                                
239300 IMS-REPL-WDGX4524 SECTION.                                               
239400     MOVE '    ' TO GODK-STATUSKODER                                      
239500     CALL CBLTDLI USING REPL 4523-PCB DLI-IO-4524                         
239600     MOVE 4523-STATUS-CODE TO STATUS-WS                                   
239700     PERFORM IMS-STATUSKONTROLL                                           
239800     .                                                                    
239900     EJECT                                                                
240000 IMS-GU-WDB201 SECTION.                                                   
240100                                                                          
240200      STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                          
240300             DELIMITED BY SIZE INTO SSA1                                  
240400      MOVE '  '                TO GODK-STATUSKODER                        
240500      CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                     
240600      MOVE WDB2-STATUS-CODE        TO STATUS-WS                           
240700      PERFORM IMS-STATUSKONTROLL                                          
240800     .                                                                    
240900     SKIP2                                                                
241000 IMS-GU-WDB101 SECTION.                                                   
241100                                                                          
241200      STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                       
241300             DELIMITED BY SIZE INTO SSA1                                  
241400      MOVE '  '                TO GODK-STATUSKODER                        
241500      CALL CBLTDLI USING GU WDB1-PCB DLI-IO-B101 SSA1                     
241600      MOVE WDB1-STATUS-CODE        TO STATUS-WS                           
241700      PERFORM IMS-STATUSKONTROLL                                          
241800     .                                                                    
241900     SKIP2                                                                
242000                                                                          
242100 IMS-GU-WDB601    SECTION.                                                
242200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
242300          DELIMITED BY SIZE INTO SSA1                                     
242400     MOVE '  '   TO GODK-STATUSKODER                                      
242500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
242600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
242700     PERFORM IMS-STATUSKONTROLL                                           
242800     .                                                                    
242900                                                                          
243000 IMS-STATUSKONTROLL SECTION.                                              
243100                                                                          
243200     SET STATUS-IX TO 1                                                   
243300     SEARCH GODK-STATUS                                                   
243400       AT END                                                             
243500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
243600           DELIMITED BY SIZE INTO FELTEXT                                 
243700         CALL FELLOG                                                      
243800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
243900         CONTINUE                                                         
244000     END-SEARCH                                                           
244100     .                                                                    
