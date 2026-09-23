000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4079900.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   00/03/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BAKGRUNDS MPP SOM SKRIVER UT KOLLIFLAGGOR.                       
000900*                                                                         
001000*        STARTAS AV PGM W40745  GENOM PROGRAM-TO-PROGRAM-SWITCH           
001100*        PGM:ET SKRIVER ENDAST KOLLI-FLAGGA (LISTA) DVS.                  
001200*        SVARAR EJ SKÄRMEN                                                
001300*                                                                         
001400*        ANROPAR W006PRS1, GENERELT LISTNINGSPROGRAM VIA SPOOL-API        
001500*        UTAN ÅTERSTART TILL PRINTER.                                     
001600*        CDC ANVÄNDER HÄR MARKPOINT-TERMO-SKRIVARE MED                    
001700*        7INCH-FORMAT (A05)                                               
001800*        NDC (JAP / AUS) ANVÄNDER HÄR ZEBRA-TERMO-SKRIVARE MED            
001900*        7INCH-FORMAT (A05)                                               
002000*                                                                         
002100*        PROGRAMMET LÄSER      WDR4                                       
002200*        PROGRAMMET LÄSER      WDB2                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T799X                                             
002600*        MID:         W4I79901                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        KOLLIFLAGGOR FÖR CDC                                             
003000*                         JAPAN                                           
003100*                         AUSTRALIEN                                      
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W4079900'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  7INCH                       PIC X       VALUE '7'.                   
005000 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
005200 77  SHIP-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005300                                                                          
005400 77  WS-IDKOLLI                  PIC S9(5)   VALUE ZERO COMP-3.           
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4799'.                
005800     EJECT                                                                
005900*      --- VALID IDDC CODES                                               
006000*                                                                         
006100*01    -COPY WWDC99                                                       
006200                                                                          
006300                                                                          
006400 01  WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                   
006500 01  FILLER REDEFINES WS-VKORDBTO.                                        
006600     03 WS-KILO                  PIC 9(6).                                
006700     03 WS-HEKTO                 PIC 9.                                   
006800                                                                          
006900     EJECT                                                                
007000 01  WS-IDPRTLST.                                                         
007100     03 WS-SYSTDEL               PIC X(1).                                
007200     03 WS-LISTTYP               PIC X(2).                                
007300     03 WS-IDDC-PR               PIC X(2).                                
007400     03 WS-KDPRT                 PIC X(3).                                
007500                                                                          
007600     EJECT                                                                
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERELLA-SUBPROGRAM.                                                
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
008200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
008300     03  WNDCADRE                PIC X(8)    VALUE 'WNDCADRE'.            
008400     EJECT                                                                
008500*                                                                         
008600 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
008700*01  -COPY W006PRT                                                        
008800*                                                                         
008900 01  FILLER                      PIC X(16)  VALUE 'WNDCADRE '.            
009000*   -COPY WNDCADRE                                                        
009100     EJECT                                                                
009200     SKIP3                                                                
009300* VARIABLER TILL SUBPROGRAM W006PRS1                                      
009400*01  -COPY W006PRAR                                                       
009500     SKIP2                                                                
009600     EJECT                                                                
009700*****************************************************************         
009800 01  FILLER                      PIC X(16)  VALUE 'LISTA-RAD'.            
009900 01  RADREDIGERING.                                                       
010000       05  STOR-RAD1.                                                     
010100           07  LISTA-IDDISTR              PIC Z(4)  VALUE ZERO.           
010200           07  LISTA-IDKUNDNR             PIC Z(5)9 VALUE ZERO.           
010300           07  LISTA-IDRAPP               PIC X(10) VALUE SPACE.          
010400           07  LISTA-IDKOLLI              PIC Z(5)  VALUE ZERO.           
010500           07  LISTA-VKORDBTO             PIC Z(5)  VALUE ZERO.           
010600       05  STOR-RAD2.                                                     
010700           07  LISTA-ADRESS-1             PIC X(30) VALUE SPACE.          
010800       05  STOR-RAD3.                                                     
010900           07  LISTA-ADRESS-2             PIC X(30) VALUE SPACE.          
011000       05  STOR-RAD4.                                                     
011100           07  LISTA-ADRESS-3             PIC X(30) VALUE SPACE.          
011200       05  STOR-RAD5.                                                     
011300           07  LISTA-ADRESS-4             PIC X(30) VALUE SPACE.          
011400           07  LISTA-FC                   PIC 9(3)  VALUE ZERO.           
011500       05  STOR-RAD6.                                                     
011600           07  LISTA-ADRESS-5             PIC X(30) VALUE SPACE.          
011700                                                                          
011800       05  STOR-RAD7.                                                     
011900           07  LISTA-ADAVS-1              PIC X(32) VALUE SPACE.          
012000       05  STOR-RAD8.                                                     
012100           07  LISTA-ADAVS-2              PIC X(32) VALUE SPACE.          
012200       05  STOR-RAD9.                                                     
012300           07  LISTA-ADAVS-3              PIC X(32) VALUE SPACE.          
012400       05  STOR-RAD10.                                                    
012500           07  LISTA-ADAVS-4              PIC X(32) VALUE SPACE.          
012600       05  STOR-RAD11.                                                    
012700           07  LISTA-ADAVS-5              PIC X(32) VALUE SPACE.          
012800                                                                          
012900******************************************************************        
013000*         PRINTRADER FÖR KOLLIFLAGGOR CDC A5-FORMAT              *        
013100*                                                                *        
013200*                                                                *        
013300*     AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.          *        
013400*     ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7INCH FORMAT I CDC       *        
013500*     CL7INCH = CASE LABEL SIZE 7INCH                            *        
013600******************************************************************        
013700 01  FILLER           PIC X(24)  VALUE 'KOLLI-FL7INCH TERMO'.             
013800*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
013900*                              LABELPOINT                                 
014000 01  CASE-LABEL-THERMO-7INCH.                                             
014100   03  CL7INCH-RAD   PIC X(132)  VALUE SPACE.                             
014200                                                                          
014300   03  CL7INCH-STYR-COBRA.                                                
014400     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
014500     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
014600     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
014700     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
014800     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
014900                                                                          
015000   03  CL7INCH-STYR-01.                                                   
015100     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
015200                                                                          
015300   03  CL7INCH-STYR-91.                                                   
015400     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
015500                                                                          
015600*-RUBRIKER                                                                
015700                                                                          
015800   03  CL7INCH-RUB-RETURN.                                                
015900     05  FILLER      PIC X(25) VALUE '!F T N  410 200  L 6 8 6 '.         
016000     05  FILLER      PIC X(11) VALUE '"RETURN"Å'.                         
016100                                                                          
016200   03  CL7INCH-RUB-1-1.                                                   
016300     05  FILLER      PIC X(25) VALUE '!F T N  510  360 R 2 1 3 '.         
016400     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
016500                                                                          
016600   03  CL7INCH-RUB-1-2.                                                   
016700     05  FILLER      PIC X(25) VALUE '!F T N  510  740 R 2 1 3 '.         
016800     05  FILLER      PIC X(09) VALUE '"DEALER"Å'.                         
016900                                                                          
017000   03  CL7INCH-RUB-1-3.                                                   
017100     05  FILLER      PIC X(25) VALUE '!F T N  510 1350 R 2 1 3 '.         
017200     05  FILLER      PIC X(16) VALUE '"REPORT NUMBER"Å'.                  
017300                                                                          
017400   03  CL7INCH-RUB-1-4.                                                   
017500     05  FILLER      PIC X(25) VALUE '!F T N  510 1750 R 2 1 3 '.         
017600     05  FILLER      PIC X(10) VALUE '"CASE NO"Å'.                        
017700                                                                          
017800   03  CL7INCH-RUB-1-5.                                                   
017900     05  FILLER      PIC X(25) VALUE '!F T N  510 2150 R 2 1 3 '.         
018000     05  FILLER      PIC X(10)  VALUE '"WEIGHT"Å'.                        
018100                                                                          
018200   03  CL7INCH-RUB-2-1.                                                   
018300     05  FILLER      PIC X(25) VALUE '!F T N  770 180  L 2 1 3 '.         
018400     05  FILLER      PIC X(12) VALUE '"CONSIGNEE"Å'.                      
018500                                                                          
018600   03  CL7INCH-RUB-3-1.                                                   
018700     05  FILLER      PIC X(25) VALUE '!F T N 1070 2150 R 2 1 6 '.         
018800     05  FILLER      PIC X(5)  VALUE '"FC"Å'.                             
018900                                                                          
019000   03  CL7INCH-RUB-4-1.                                                   
019100     05  FILLER      PIC X(25) VALUE '!F T N 1350 180  L 2 1 3 '.         
019200     05  FILLER      PIC X(10) VALUE '"SHIPPER"Å'.                        
019300                                                                          
019400*-FÄLTDATA                                                                
019500                                                                          
019600   03  CL7INCH-DATA-1-1.                                                  
019700     05  FILLER      PIC X(25) VALUE '!F T N  660  360 R 2 1 6 '.         
019800     05  FILLER      PIC X(1)  VALUE '"'.                                 
019900     05  CL7INCH-IDDISTR PIC Z(4) VALUE ZERO.                             
020000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020100                                                                          
020200   03  CL7INCH-DATA-1-2.                                                  
020300     05  FILLER      PIC X(25) VALUE '!F T N  660  740 R 2 1 6 '.         
020400     05  FILLER      PIC X(1)  VALUE '"'.                                 
020500     05  CL7INCH-IDKUNDNR PIC Z(5)9 VALUE ZERO.                           
020600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
020700                                                                          
020800   03  CL7INCH-DATA-1-3.                                                  
020900     05  FILLER      PIC X(25) VALUE '!F T N  660 1350 R 2 1 6 '.         
021000     05  FILLER      PIC X(1)  VALUE '"'.                                 
021100     05  CL7INCH-IDRAPP  PIC X(10) VALUE SPACE.                           
021200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
021300                                                                          
021400   03  CL7INCH-DATA-1-4.                                                  
021500     05  FILLER      PIC X(25) VALUE '!F T N  660 1750 R 2 1 6 '.         
021600     05  FILLER      PIC X(1)  VALUE '"'.                                 
021700     05  CL7INCH-IDKOLLI PIC Z(5) VALUE ZERO.                             
021800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
021900                                                                          
022000   03  CL7INCH-DATA-KILO-HEKTO.                                           
022100     05  FILLER      PIC X(25) VALUE '!F T N  660 2150 R 2 1 6 '.         
022200     05  FILLER      PIC X(1)  VALUE '"'.                                 
022300     05  CL7INCH-KILO     PIC Z(5)9  VALUE ZERO.                          
022400     05  CL7INCH-PUNKT    PIC X      VALUE '.'.                           
022500     05  CL7INCH-HEKTO    PIC 9      VALUE ZERO.                          
022600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
022700                                                                          
022800                                                                          
022900*-GODSMOTTAGARADRESS                                                      
023000                                                                          
023100   03  CL7INCH-ADRESS-2-1.                                                
023200     05  FILLER      PIC X(25) VALUE '!F T N  870 190  L 3 2 3 '.         
023300     05  FILLER      PIC X(1)  VALUE '"'.                                 
023400     05  CL7INCH-ADRESS-1 PIC X(30) VALUE SPACE.                          
023500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
023600                                                                          
023700   03  CL7INCH-ADRESS-2-2.                                                
023800     05  FILLER      PIC X(25) VALUE '!F T N  970 190  L 3 2 3 '.         
023900     05  FILLER      PIC X(1)  VALUE '"'.                                 
024000     05  CL7INCH-ADRESS-2 PIC X(30) VALUE SPACE.                          
024100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024200                                                                          
024300   03  CL7INCH-ADRESS-2-3.                                                
024400     05  FILLER      PIC X(25) VALUE '!F T N 1070 190  L 3 2 3 '.         
024500     05  FILLER      PIC X(1)  VALUE '"'.                                 
024600     05  CL7INCH-ADRESS-3 PIC X(30) VALUE SPACE.                          
024700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
024800                                                                          
024900   03  CL7INCH-ADRESS-2-4.                                                
025000     05  FILLER      PIC X(25) VALUE '!F T N 1170 190  L 3 2 3 '.         
025100     05  FILLER      PIC X(1)  VALUE '"'.                                 
025200     05  CL7INCH-ADRESS-4 PIC X(30) VALUE SPACE.                          
025300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
025400                                                                          
025500   03  CL7INCH-FC-1.                                                      
025600     05  FILLER      PIC X(25) VALUE '!F T N 1270 2150 R 3 2 6 '.         
025700     05  FILLER      PIC X(1)  VALUE '"'.                                 
025800     05  CL7INCH-FC  PIC Z(3)  VALUE ZERO.                                
025900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
026000                                                                          
026100   03  CL7INCH-ADRESS-2-5.                                                
026200     05  FILLER      PIC X(25) VALUE '!F T N 1270 190  L 3 2 3 '.         
026300     05  FILLER      PIC X(1)  VALUE '"'.                                 
026400     05  CL7INCH-ADRESS-5 PIC X(30) VALUE SPACE.                          
026500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
026600                                                                          
026700*-AVSÄNDARADRESS - CDC / NDC                                              
026800                                                                          
026900   03  CL7INCH-ADRESS1-AVS.                                               
027000     05  FILLER      PIC X(25) VALUE '!F T N 1450 190  L 2 1 3 '.         
027100     05  FILLER      PIC X(1)  VALUE '"'.                                 
027200     05  CL7INCH-ADRESS1 PIC X(32) VALUE SPACE.                           
027300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
027400                                                                          
027500   03  CL7INCH-ADRESS2-AVS.                                               
027600     05  FILLER      PIC X(25) VALUE '!F T N 1530 190  L 2 1 3 '.         
027700     05  FILLER      PIC X(1)  VALUE '"'.                                 
027800     05  CL7INCH-ADRESS2 PIC X(32) VALUE SPACE.                           
027900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028000                                                                          
028100   03  CL7INCH-ADRESS4-AVS.                                               
028200     05  FILLER      PIC X(25) VALUE '!F T N 1610 190  L 2 1 3 '.         
028300     05  FILLER      PIC X(1)  VALUE '"'.                                 
028400     05  CL7INCH-ADRESS4 PIC X(32) VALUE SPACE.                           
028500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
028600                                                                          
028700   03  CL7INCH-ADRESS5-AVS.                                               
028800     05  FILLER      PIC X(25) VALUE '!F T N 1690 190  L 2 1 3 '.         
028900     05  FILLER      PIC X(1)  VALUE '"'.                                 
029000     05  CL7INCH-ADRESS5  PIC X(32) VALUE SPACE.                          
029100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
029200                                                                          
029300     EJECT                                                                
029400******************************************************************        
029500*        SLUT PÅ CDC KOLLIFLAGGA, MARKPOINTSKRIVARE   A05        *        
029600******************************************************************        
029700                                                                          
029800                                                                          
029900                                                                          
030000******************************************************************        
030100*        AREA MED STYRTECKEN FÖR ZEBRA TERMO SKRIVARE.           *        
030200*        ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7INCH FORMAT I NDC.   *        
030300******************************************************************        
030400 01  FILLER                  PIC X(16)  VALUE 'ZEBRA PRINT NDC '.         
030500*    --- CASE LABEL 7INCH FORMAT FOR ZEBRA PRINTER JAP/AUS                
030600*                                                                         
030700 01      KOLLI-FLAGGA-ZEBRA.                                              
030800   03    KF-ZEBRA-RAD            PIC X(132)   VALUE  SPACE.               
030900*                                                                         
031000*    --- STEERING LINES FOR ZEBRA                                         
031100*                                                                         
031200   03    KF-ZEBRA-STYR-01        PIC X(80)   VALUE                        
031300         '^XA^CF0^FWR^FS                                      '.          
031400   03    KF-ZEBRA-STYR-03        PIC X(80)   VALUE                        
031500         '^XZ                                                 '.          
031600*                                                                         
031700*    --- TEXT LINES                                                       
031800*                                                                         
031900   03    KF-ZEBRA-RUB-RETURN     PIC X(80)   VALUE                        
032000         '^FO0090,0050^A0N,350,445^FDRETURN^FS                 '.         
032100                                                                          
032200   03    KF-ZEBRA-RUB-IDDISTR    PIC X(80)   VALUE                        
032300         '^FO0070,0370^A0N,40,30^FDDISTRICT^FS                 '.         
032400                                                                          
032500   03    KF-ZEBRA-RUB-IDKUND     PIC X(80)   VALUE                        
032600         '^FO0300,0370^A0N,40,30^FDDEALER^FS                  '.          
032700   03    KF-ZEBRA-RUB-IDRAPP     PIC X(80)   VALUE                        
032800         '^FO0600,0370^A0N,40,30^FDREPORT NUMBER^FS           '.          
032900   03    KF-ZEBRA-RUB-IDKOLLI    PIC X(80)   VALUE                        
033000         '^FO1130,0370^A0N,40,30^FDCASE NO^FS                 '.          
033100                                                                          
033200   03    KF-ZEBRA-RUB-VKORDBTO   PIC X(80)   VALUE                        
033300         '^FO1495,0370^A0N,40,30^FDWEIGHT^FS                  '.          
033400                                                                          
033500   03    KF-ZEBRA-RUB-ADRESS     PIC X(80)   VALUE                        
033600         '^FO0070,0580^A0N,40,30^FDCONSIGNEE^FS               '.          
033700   03    KF-ZEBRA-RUB-SHIPPER       PIC X(80)   VALUE                     
033800         '^FO0070,1010^A0N,0040,0030^FDSHIPPER^FS              '.         
033900                                                                          
034000*                                                                         
034100*    --- DATA FOR LABEL                                                   
034200*                                                                         
034300                                                                          
034400   03    KF-ZEBRA-DATA-IDDISTR.                                           
034500     05    FILLER                  PIC X(27)   VALUE                      
034600         '^FO0070,0430^A0N,110,080^FD'.                                   
034700     05    KF-ZEBRA-IDDISTR        PIC Z(3)9 VALUE ZERO.                  
034800     05    FILLER                  PIC X(49)   VALUE                      
034900         '^FS                       '.                                    
035000   03    KF-ZEBRA-DATA-IDKUNDNR.                                          
035100     05    FILLER                  PIC X(27)   VALUE                      
035200         '^FO0300,0430^A0N,110,080^FD'.                                   
035300     05    KF-ZEBRA-IDKUNDNR       PIC Z(5)9 VALUE ZERO.                  
035400     05    FILLER                  PIC X(47)   VALUE                      
035500         '^FS                       '.                                    
035600   03    KF-ZEBRA-DATA-IDRAPP.                                            
035700     05    FILLER                  PIC X(27)   VALUE                      
035800         '^FO0600,0430^A0N,110,080^FD'.                                   
035900     05    KF-ZEBRA-IDRAPP         PIC X(10) VALUE SPACE.                 
036000     05    FILLER                  PIC X(48)   VALUE                      
036100         '^FS                       '.                                    
036200*                                                                         
036300   03    KF-ZEBRA-DATA-IDKOLLI.                                           
036400     05    FILLER                  PIC X(27)   VALUE                      
036500         '^FO1100,0430^A0N,110,080^FD'.                                   
036600     05    KF-ZEBRA-IDKOLLI        PIC Z(4)9 VALUE ZERO.                  
036700     05    FILLER                  PIC X(49)   VALUE                      
036800         '^FS                       '.                                    
036900*                                                                         
037000   03  KF-ZEBRA-DATA-WEIGHT.                                              
037100     05    FILLER                  PIC X(27)   VALUE                      
037200         '^FO1350,0430^A0N,110,080^FD'.                                   
037300     05    KF-ZEBRA-KILO PIC Z(5)9 VALUE ZERO.                            
037400     05    KF-ZEBRA-PUNKT PIC X VALUE '.'.                                
037500     05    KF-ZEBRA-HEKTO PIC 9 VALUE ZERO.                               
037600     05    FILLER                  PIC X(49)   VALUE                      
037700         '^FS                       '.                                    
037800*                                                                         
037900   03    KF-ZEBRA-DATA-ADRESS-1.                                          
038000     05    FILLER                  PIC X(29)   VALUE                      
038100         '^FO0070,0650^A0N,0058,0055^FD'.                                 
038200     05    KF-ZEBRA-ADRESS-1       PIC X(27).                             
038300     05    FILLER                  PIC X(26)   VALUE                      
038400         '^FS                    '.                                       
038500                                                                          
038600   03    KF-ZEBRA-DATA-ADRESS-2.                                          
038700     05    FILLER                  PIC X(29)   VALUE                      
038800         '^FO0070,0720^A0N,0058,0055^FD'.                                 
038900     05    KF-ZEBRA-ADRESS-2       PIC X(27).                             
039000     05    FILLER                  PIC X(26)   VALUE                      
039100         '^FS                    '.                                       
039200                                                                          
039300   03    KF-ZEBRA-DATA-ADRESS-3.                                          
039400     05    FILLER                  PIC X(29)   VALUE                      
039500         '^FO0070,0790^A0N,0058,0055^FD'.                                 
039600     05    KF-ZEBRA-ADRESS-3       PIC X(27).                             
039700     05    FILLER                  PIC X(26)   VALUE                      
039800         '^FS                    '.                                       
039900                                                                          
040000   03    KF-ZEBRA-DATA-ADRESS-4.                                          
040100     05    FILLER                  PIC X(29)   VALUE                      
040200         '^FO0070,0860^A0N,0058,0055^FD'.                                 
040300     05    KF-ZEBRA-ADRESS-4       PIC X(27).                             
040400     05    FILLER                  PIC X(26)   VALUE                      
040500         '^FS                    '.                                       
040600                                                                          
040700   03    KF-ZEBRA-DATA-ADRESS-5.                                          
040800     05    FILLER                  PIC X(29)   VALUE                      
040900         '^FO0070,0930^A0N,0058,0055^FD'.                                 
041000     05    KF-ZEBRA-ADRESS-5       PIC X(27).                             
041100     05    FILLER                  PIC X(26)   VALUE                      
041200         '^FS                    '.                                       
041300                                                                          
041400*SHIPPER                                                                  
041500   03  KF-ZEBRA-DATA-SHIPPER-COMPANY.                                     
041600     05    FILLER                  PIC X(29)   VALUE                      
041700         '^FO0070,1020^A0N,0040,0035^FD'.                                 
041800     05    KF-ZEBRA-SHIPPER-COMPANY PIC X(32).                            
041900     05    FILLER                  PIC X(21)   VALUE                      
042000         '^FS                  '.                                         
042100*                                                                         
042200   03  KF-ZEBRA-DATA-SHIPPER-NAME.                                        
042300     05    FILLER                  PIC X(29)   VALUE                      
042400         '^FO0070,1090^A0N,0040,0035^FD'.                                 
042500     05    KF-ZEBRA-SHIPPER-NAME   PIC X(32).                             
042600     05    FILLER                  PIC X(21)   VALUE                      
042700         '^FS                  '.                                         
042800*                                                                         
042900   03  KF-ZEBRA-DATA-SHIPPER-STREET.                                      
043000     05    FILLER                  PIC X(29)   VALUE                      
043100         '^FO0070,1150^A0N,0040,0035^FD'.                                 
043200     05    KF-ZEBRA-SHIPPER-STREET PIC X(32).                             
043300     05    FILLER                  PIC X(21)   VALUE                      
043400         '^FS                  '.                                         
043500*                                                                         
043600   03  KF-ZEBRA-DATA-SHIPPER-CITY.                                        
043700     05    FILLER                  PIC X(29)   VALUE                      
043800         '^FO0070,1210^A0N,0040,0035^FD'.                                 
043900     05    KF-ZEBRA-SHIPPER-CITY   PIC X(32).                             
044000     05    FILLER                  PIC X(21)   VALUE                      
044100         '^FS                  '.                                         
044200*                                                                         
044300   03  KF-ZEBRA-DATA-SHIPPER-COUNTRY.                                     
044400     05    FILLER                  PIC X(29)   VALUE                      
044500         '^FO0070,1270^A0N,0040,0035^FD'.                                 
044600     05    KF-ZEBRA-SHIPPER-COUNTRY PIC X(32).                            
044700     05    FILLER                   PIC X(21)  VALUE                      
044800         '^FS                  '.                                         
044900*                                                                         
045000******************************************************************        
045100*    END OF JAP/AUS CASELABEL , ZEBRA A05                        *        
045200******************************************************************        
045300     EJECT                                                                
045400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
045500*                                                                         
045600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
045700     SKIP3                                                                
045800*01  MID -COPY W4I79901                                                   
045900     EJECT                                                                
046000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
046100     SKIP3                                                                
046200*01  -COPY WMSGAREA                                                       
046300     EJECT                                                                
046400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046500*                                                                         
046600     EJECT                                                                
046700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046800     SKIP3                                                                
046900 01  NYCKLAR-TILL-DLI.                                                    
047000     03  W-WDGX4101-X.                                                    
047100         05 W-4101-IDHTYP          PIC X(4)    VALUE '4101'.              
047200         05 W-4101-IDDC            PIC X(2)    VALUE SPACE.               
047300         05 W-4101-IDDISTR         PIC S9(5)   VALUE ZERO COMP-3.         
047400         05 W-4101-LOWVALUE        PIC X(21)   VALUE LOW-VALUE.           
047500                                                                          
047600     03  W-WDGX4102-MIN-X.                                                
047700         05 W-4102-IDKUNDNR-MIN    PIC S9(7)   VALUE ZERO COMP-3.         
047800         05 W-4102-IDRAPP-MIN      PIC X(10)   VALUE SPACE.               
047900         05 W-4102-IDKOLLI-MIN     PIC S9(5)   VALUE ZERO COMP-3.         
048000         05 W-4102-IDARTNR-MIN     PIC S9(9)   VALUE ZERO COMP-3.         
048100                                                                          
048200     03  W-WDGX4102-MAX-X.                                                
048300         05 W-4102-IDKUNDNR-MAX    PIC S9(7)   VALUE ZERO COMP-3.         
048400         05 W-4102-IDRAPP-MAX      PIC X(10)   VALUE SPACE.               
048500         05 W-4102-IDKOLLI-MAX     PIC S9(5) VALUE +99999 COMP-3.         
048600         05 W-4102-IDARTNR-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
048700                                                                          
048800     03  W-IDGMT-X.                                                       
048900         05 W-IDDISTR-WDB2         PIC S9(5)   VALUE ZERO COMP-3.         
049000         05 W-IDKUNDNR-WDB2        PIC S9(7)   VALUE ZERO COMP-3.         
049100                                                                          
049200     03  W-WDB301KY-X.                                                    
049300         05  W-IDDC-WDB3           PIC X(2)    VALUE SPACE.               
049400         05  W-IDDISTR-WDB3        PIC S9(5)   VALUE ZERO COMP-3.         
049500         05  W-IDKUNDNR-WDB3       PIC S9(7)   VALUE ZERO COMP-3.         
049600                                                                          
049700     03  W-WDB301KY-DEF-X.                                                
049800         05  W-IDDC-WDB3-DEF       PIC X(2)    VALUE SPACE.               
049900         05  W-IDDISTR-WDB3-DEF    PIC S9(5)   VALUE ZERO COMP-3.         
050000         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
050100                                                                          
050200     SKIP2                                                                
050300*    --- STATUS-KOD FRÅN IMS                                              
050400 01  STATUS-WS                   PIC XX.                                  
050500     88  SEGMENT-FINNS                       VALUE '  '.                  
050600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
050700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
050800     SKIP2                                                                
050900 01  GODK-STATUSKODER.                                                    
051000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
051100     SKIP3                                                                
051200 01  SSA1                        PIC X(96).                               
051300 01  SSA2                        PIC X(96).                               
051400     EJECT                                                                
051500*    --- IMS FUNKTIONSKODER                                               
051600*01  -COPY W0003                                                          
051700     EJECT                                                                
051800*    ---  DLI INPUT-OUTPUT AREA                                           
051900                                                                          
052000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4101'.                    
052100 01  DLI-IO-WDGX4101.                                                     
052200*    03  -COPY WDGX4101                                                   
052300     EJECT                                                                
052400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4102'.                    
052500 01  DLI-IO-WDGX4102.                                                     
052600*    03  -COPY WDGX4102                                                   
052700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
052800 01  DLI-IO-WDB201.                                                       
052900*    03  -COPY WDB201                                                     
053000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB301'.                      
053100 01  DLI-IO-WDB301.                                                       
053200*    03  -COPY WDB301                                                     
053300     EJECT                                                                
053400     EJECT                                                                
053500 LINKAGE SECTION.                                                         
053600*01  -COPY W0009   -PRE MSG-                                              
053700     EJECT                                                                
053800*01  -COPY W0009   -PRE ALT-                                              
053900     EJECT                                                                
054000*01  -COPY W0008   -PRE USEA-                                             
054100     05  FILLER                  PIC X.                                   
054200                                                                          
054300*01  -COPY W0008  -PRE 4101-                                              
054400     05  FILLER                  PIC X.                                   
054500                                                                          
054600*01  -COPY W0008  -PRE WDB2-                                              
054700     05  FILLER                  PIC X.                                   
054800                                                                          
054900*01  -COPY W0008  -PRE WDB3-                                              
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 4101-PCB              
055300                           WDB2-PCB WDB3-PCB.                             
055400 MAIN SECTION.                                                            
055500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 4101-PCB              
055600                           WDB2-PCB WDB3-PCB.                             
055700                                                                          
055800     PERFORM IMS-GET-MSG                                                  
055900     IF SEGMENT-FINNS                                                     
056000       PERFORM A-INIT                                                     
056100                                                                          
056200       EVALUATE TRUE                                                      
056300          WHEN CDC-SE                                                     
056400           PERFORM B-SKAPA-KOLLIFLAGGOR-CDC                               
056500          WHEN NDC-JP OR NDC-AU                                           
056600           PERFORM C-SKAPA-KOLLIFLAGGOR-NDC                               
056700       END-EVALUATE                                                       
056800     END-IF                                                               
056900                                                                          
057000     PERFORM Z-FINIT                                                      
057100     MOVE ZERO TO RETURN-CODE                                             
057200     GOBACK                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 A-INIT SECTION.                                                          
057600                                                                          
057700     IF MSG-DUBBLA-TRANSKODER                                             
057800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I79901                 
057900     ELSE                                                                 
058000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I79901                  
058100     END-IF                                                               
058200                                                                          
058300     MOVE LOW-VALUE TO MSG-AREA                                           
058400                                                                          
058500     MOVE MID-IDDC      TO W-4101-IDDC                                    
058600                           W-IDDC-WDB3                                    
058700                           W-IDDC-WDB3-DEF                                
058800                                                                          
058900     MOVE MID-IDDISTR   TO W-4101-IDDISTR                                 
059000                           W-IDDISTR-WDB2                                 
059100                           W-IDDISTR-WDB3                                 
059200                           W-IDDISTR-WDB3-DEF                             
059300                                                                          
059400     MOVE MID-IDKUNDNR  TO W-4102-IDKUNDNR-MIN                            
059500                           W-4102-IDKUNDNR-MAX                            
059600                           W-IDKUNDNR-WDB2                                
059700                           W-IDKUNDNR-WDB3                                
059800                                                                          
059900     MOVE MID-IDRAPP    TO W-4102-IDRAPP-MIN                              
060000                           W-4102-IDRAPP-MAX                              
060100                                                                          
060200     PERFORM AA-OPEN-PRINTER                                              
060300     .                                                                    
060400     EJECT                                                                
060500 AA-OPEN-PRINTER         SECTION.                                         
060600                                                                          
060700     MOVE MID-IDDC           TO WS-IDDC-PR                                
060800                                WS-IDDC                                   
060900                                                                          
061000     EVALUATE TRUE                                                        
061100        WHEN CDC-SE                                                       
061200           MOVE '4'                TO WS-SYSTDEL                          
061300           MOVE 'KF'               TO WS-LISTTYP                          
061400           MOVE '9  '              TO WS-KDPRT                            
061500        WHEN NDC-JP                                                       
061600           MOVE '4'                TO WS-SYSTDEL                          
061700           MOVE 'KF'               TO WS-LISTTYP                          
061800           MOVE '4  '              TO WS-KDPRT                            
061900        WHEN NDC-AU                                                       
062000           MOVE '6'                TO WS-SYSTDEL                          
062100           MOVE 'FA'               TO WS-LISTTYP                          
062200           MOVE 'U '               TO WS-IDDC-PR                          
062300           MOVE SPACE              TO WS-KDPRT                            
062400     END-EVALUATE                                                         
062500                                                                          
062600     MOVE WS-IDPRTLST        TO PRT-IDPRTLST                              
062700                                                                          
062800     IF CDC-SE                                                            
062900       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
063000                           PRT-OPEN                                       
063100                           PRT-IDPRTLST                                   
063200                           ALT-PCB                                        
063300                           DUMMY-AREA                                     
063400                           DUMMY-AREA                                     
063500     ELSE                                                                 
063600       CALL W006PRS1 USING PRT-SPOOL-A4S                                  
063700                           PRT-OPEN                                       
063800                           PRT-IDPRTLST                                   
063900                           ALT-PCB                                        
064000                           DUMMY-AREA                                     
064100                           DUMMY-AREA                                     
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 B-SKAPA-KOLLIFLAGGOR-CDC SECTION.                                        
064600                                                                          
064700     PERFORM IMS-GU-WDB201                                                
064800     IF SEGMENT-FINNS                                                     
064900       MOVE GMT-BEGMT-RAD1     TO LISTA-ADRESS-1                          
065000       MOVE GMT-BEGMT-RAD2     TO LISTA-ADRESS-2                          
065100       MOVE GMT-ADGMT-GATA     TO LISTA-ADRESS-3                          
065200       MOVE GMT-ADGMT-PADR     TO LISTA-ADRESS-4                          
065300       MOVE GMT-ADGMT-LAND     TO LISTA-ADRESS-5                          
065400     END-IF                                                               
065500                                                                          
065600     PERFORM IMS-GU-WDB301                                                
065700     IF SEGMENT-FINNS                                                     
065800       MOVE DC-KDGENFRA-MO     TO LISTA-FC                                
065900     END-IF                                                               
066000                                                                          
066100     PERFORM IMS-GU-WDGX4101                                              
066200                                                                          
066300     IF SEGMENT-FINNS                                                     
066400       PERFORM IMS-GNP-WDGX4102                                           
066500                                                                          
066600       IF SEGMENT-FINNS                                                   
066700                                                                          
066800         MOVE 4102-IDKOLLI    TO WS-IDKOLLI                               
066900         PERFORM BA-FLYTTA-DATA                                           
067000         PERFORM S01-PRINT-MARKPOINT-TERMO7INC                            
067100                                                                          
067200         PERFORM UNTIL SEGMENT-SAKNAS                                     
067300           IF 4102-IDKOLLI = WS-IDKOLLI                                   
067400               CONTINUE                                                   
067500           ELSE                                                           
067600             MOVE 4102-IDKOLLI    TO WS-IDKOLLI                           
067700                                                                          
067800             MOVE 4102-IDKOLLI          TO LISTA-IDKOLLI                  
067900             MOVE 4102-VKORDBTO-KOLLI   TO LISTA-VKORDBTO                 
068000                                           WS-VKORDBTO                    
068100                                                                          
068200             PERFORM S01-PRINT-MARKPOINT-TERMO7INC                        
068300           END-IF                                                         
068400                                                                          
068500           PERFORM IMS-GNP-WDGX4102                                       
068600         END-PERFORM                                                      
068700                                                                          
068800       END-IF                                                             
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 BA-FLYTTA-DATA     SECTION.                                              
069300                                                                          
069400     MOVE 4101-IDDISTR            TO LISTA-IDDISTR                        
069500     MOVE 4102-IDKUNDNR           TO LISTA-IDKUNDNR                       
069600     MOVE 4102-IDRAPP             TO LISTA-IDRAPP                         
069700     MOVE 4102-IDKOLLI            TO LISTA-IDKOLLI                        
069800     MOVE 4102-VKORDBTO-KOLLI     TO LISTA-VKORDBTO                       
069900                                     WS-VKORDBTO                          
070000                                                                          
070100*SHIPPER-INFO FINNS I SHIPPER-TAB I PROGRAMMET.                           
070200                                                                          
070300     IF CDC-SE                                                            
070400       MOVE 'VOLVO CAR CORPORATION         '                              
070500                                     TO LISTA-ADAVS-1                     
070600       MOVE 'CUSTOMER SERVICE              '                              
070700                                     TO LISTA-ADAVS-2                     
070800       MOVE 'SE-405 31 GOTHENBURG          '                              
070900                                     TO LISTA-ADAVS-4                     
071000       MOVE 'SWEDEN                        '                              
071100                                     TO LISTA-ADAVS-5                     
071200                                                                          
071300     ELSE                                                                 
071400       EVALUATE TRUE                                                      
071500         WHEN NDC-JP                                                      
071600           MOVE +8                     TO SHIP-INDX                       
071700         WHEN NDC-AU                                                      
071800           MOVE +9                     TO SHIP-INDX                       
071900       END-EVALUATE                                                       
072000                                                                          
072100       MOVE SHIPPER-COMPANY (SHIP-INDX)  TO LISTA-ADAVS-1                 
072200       MOVE SHIPPER-NAME (SHIP-INDX)     TO LISTA-ADAVS-2                 
072300       MOVE SHIPPER-STREET (SHIP-INDX)   TO LISTA-ADAVS-3                 
072400       MOVE SHIPPER-CITY (SHIP-INDX)     TO LISTA-ADAVS-4                 
072500       MOVE SHIPPER-COUNTRY (SHIP-INDX)  TO LISTA-ADAVS-5                 
072600     END-IF                                                               
072700                                                                          
072800     .                                                                    
072900     EJECT                                                                
073000 C-SKAPA-KOLLIFLAGGOR-NDC SECTION.                                        
073100                                                                          
073200     PERFORM IMS-GU-WDB201                                                
073300     IF SEGMENT-FINNS                                                     
073400       MOVE GMT-BEGMT-RAD1     TO LISTA-ADRESS-1                          
073500       MOVE GMT-BEGMT-RAD2     TO LISTA-ADRESS-2                          
073600       MOVE GMT-ADGMT-GATA     TO LISTA-ADRESS-3                          
073700       MOVE GMT-ADGMT-PADR     TO LISTA-ADRESS-4                          
073800       MOVE GMT-ADGMT-LAND     TO LISTA-ADRESS-5                          
073900     END-IF                                                               
074000                                                                          
074100     PERFORM IMS-GU-WDGX4101                                              
074200                                                                          
074300     IF SEGMENT-FINNS                                                     
074400       PERFORM IMS-GNP-WDGX4102                                           
074500                                                                          
074600       IF SEGMENT-FINNS                                                   
074700                                                                          
074800         MOVE 4102-IDKOLLI    TO WS-IDKOLLI                               
074900         PERFORM BA-FLYTTA-DATA                                           
075000         PERFORM S03-PRINT-ZEBRA-TERMO7INC                                
075100                                                                          
075200         PERFORM UNTIL SEGMENT-SAKNAS                                     
075300           IF 4102-IDKOLLI = WS-IDKOLLI                                   
075400               CONTINUE                                                   
075500           ELSE                                                           
075600             MOVE 4102-IDKOLLI    TO WS-IDKOLLI                           
075700                                                                          
075800             MOVE 4102-IDKOLLI          TO LISTA-IDKOLLI                  
075900             MOVE 4102-VKORDBTO-KOLLI   TO LISTA-VKORDBTO                 
076000                                           WS-VKORDBTO                    
076100                                                                          
076200             PERFORM S03-PRINT-ZEBRA-TERMO7INC                            
076300           END-IF                                                         
076400                                                                          
076500           PERFORM IMS-GNP-WDGX4102                                       
076600         END-PERFORM                                                      
076700                                                                          
076800       END-IF                                                             
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 Z-FINIT                   SECTION.                                       
077300                                                                          
077400     IF CDC-SE                                                            
077500       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
077600                           PRT-CLOSE                                      
077700                           PRT-IDPRTLST                                   
077800                           ALT-PCB                                        
077900                           DUMMY-AREA                                     
078000                           DUMMY-AREA                                     
078100     ELSE                                                                 
078200       CALL W006PRS1 USING PRT-SPOOL-A4S                                  
078300                           PRT-CLOSE                                      
078400                           PRT-IDPRTLST                                   
078500                           ALT-PCB                                        
078600                           DUMMY-AREA                                     
078700                           DUMMY-AREA                                     
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100 S01-PRINT-MARKPOINT-TERMO7INC    SECTION.                                
079200                                                                          
079300     MOVE LISTA-IDDISTR   TO CL7INCH-IDDISTR                              
079400     MOVE LISTA-IDKUNDNR  TO CL7INCH-IDKUNDNR                             
079500     MOVE LISTA-IDRAPP    TO CL7INCH-IDRAPP                               
079600     MOVE LISTA-IDKOLLI   TO CL7INCH-IDKOLLI                              
079700     MOVE WS-KILO         TO CL7INCH-KILO                                 
079800     MOVE WS-HEKTO        TO CL7INCH-HEKTO                                
079900     MOVE LISTA-FC        TO CL7INCH-FC                                   
080000                                                                          
080100     MOVE LISTA-ADRESS-1  TO CL7INCH-ADRESS-1                             
080200     MOVE LISTA-ADRESS-2  TO CL7INCH-ADRESS-2                             
080300     MOVE LISTA-ADRESS-3  TO CL7INCH-ADRESS-3                             
080400     MOVE LISTA-ADRESS-4  TO CL7INCH-ADRESS-4                             
080500     MOVE LISTA-ADRESS-5  TO CL7INCH-ADRESS-5                             
080600                                                                          
080700     MOVE LISTA-ADAVS-1   TO CL7INCH-ADRESS1                              
080800     MOVE LISTA-ADAVS-2   TO CL7INCH-ADRESS2                              
080900     MOVE LISTA-ADAVS-4   TO CL7INCH-ADRESS4                              
081000     MOVE LISTA-ADAVS-5   TO CL7INCH-ADRESS5                              
081100                                                                          
081200     MOVE SPACE           TO CL7INCH-RAD                                  
081300                                                                          
081400     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
081500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE PRT-IDPRTLST             
081600                           ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD            
081700                                                                          
081800                                                                          
081900     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
082000     PERFORM S02-SKRIV-RAD                                                
082100                                                                          
082200     MOVE CL7INCH-RUB-RETURN TO CL7INCH-RAD                               
082300     PERFORM S02-SKRIV-RAD                                                
082400                                                                          
082500     MOVE CL7INCH-RUB-1-1 TO CL7INCH-RAD                                  
082600     PERFORM S02-SKRIV-RAD                                                
082700                                                                          
082800     MOVE CL7INCH-RUB-1-2 TO CL7INCH-RAD                                  
082900     PERFORM S02-SKRIV-RAD                                                
083000                                                                          
083100     MOVE CL7INCH-RUB-1-3 TO CL7INCH-RAD                                  
083200     PERFORM S02-SKRIV-RAD                                                
083300                                                                          
083400     MOVE CL7INCH-RUB-1-4 TO CL7INCH-RAD                                  
083500     PERFORM S02-SKRIV-RAD                                                
083600                                                                          
083700     MOVE CL7INCH-RUB-1-5 TO CL7INCH-RAD                                  
083800     PERFORM S02-SKRIV-RAD                                                
083900                                                                          
084000     MOVE CL7INCH-RUB-2-1 TO CL7INCH-RAD                                  
084100     PERFORM S02-SKRIV-RAD                                                
084200                                                                          
084300     MOVE CL7INCH-RUB-3-1 TO CL7INCH-RAD                                  
084400     PERFORM S02-SKRIV-RAD                                                
084500                                                                          
084600     MOVE CL7INCH-RUB-4-1 TO CL7INCH-RAD                                  
084700     PERFORM S02-SKRIV-RAD                                                
084800                                                                          
084900     MOVE CL7INCH-DATA-1-1 TO CL7INCH-RAD                                 
085000     PERFORM S02-SKRIV-RAD                                                
085100                                                                          
085200     MOVE CL7INCH-DATA-1-2 TO CL7INCH-RAD                                 
085300     PERFORM S02-SKRIV-RAD                                                
085400                                                                          
085500     MOVE CL7INCH-DATA-1-3 TO CL7INCH-RAD                                 
085600     PERFORM S02-SKRIV-RAD                                                
085700                                                                          
085800     MOVE CL7INCH-DATA-1-4 TO CL7INCH-RAD                                 
085900     PERFORM S02-SKRIV-RAD                                                
086000                                                                          
086100     MOVE CL7INCH-DATA-KILO-HEKTO TO CL7INCH-RAD                          
086200     PERFORM S02-SKRIV-RAD                                                
086300                                                                          
086400     MOVE CL7INCH-ADRESS-2-1 TO CL7INCH-RAD                               
086500     PERFORM S02-SKRIV-RAD                                                
086600                                                                          
086700     MOVE CL7INCH-ADRESS-2-2 TO CL7INCH-RAD                               
086800     PERFORM S02-SKRIV-RAD                                                
086900                                                                          
087000     MOVE CL7INCH-ADRESS-2-3 TO CL7INCH-RAD                               
087100     PERFORM S02-SKRIV-RAD                                                
087200                                                                          
087300     MOVE CL7INCH-ADRESS-2-4 TO CL7INCH-RAD                               
087400     PERFORM S02-SKRIV-RAD                                                
087500                                                                          
087510     MOVE CL7INCH-FC-1       TO CL7INCH-RAD                               
087520     PERFORM S02-SKRIV-RAD                                                
087530                                                                          
087600     MOVE CL7INCH-ADRESS-2-5 TO CL7INCH-RAD                               
087700     PERFORM S02-SKRIV-RAD                                                
087800                                                                          
087900     MOVE CL7INCH-ADRESS1-AVS  TO CL7INCH-RAD                             
088000     PERFORM S02-SKRIV-RAD                                                
088100                                                                          
088200     MOVE CL7INCH-ADRESS2-AVS  TO CL7INCH-RAD                             
088300     PERFORM S02-SKRIV-RAD                                                
088400                                                                          
088500     MOVE CL7INCH-ADRESS4-AVS  TO CL7INCH-RAD                             
088600     PERFORM S02-SKRIV-RAD                                                
088700                                                                          
088800     MOVE CL7INCH-ADRESS5-AVS  TO CL7INCH-RAD                             
088900     PERFORM S02-SKRIV-RAD                                                
089000                                                                          
089100                                                                          
089200     MOVE CL7INCH-STYR-91 TO CL7INCH-RAD                                  
089300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE PRT-IDPRTLST             
089400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
089500                                                                          
089600     .                                                                    
089700     EJECT                                                                
089800 S02-SKRIV-RAD SECTION.                                                   
089900                                                                          
090000     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
090100                         PRT-WRITE                                        
090200                         PRT-IDPRTLST                                     
090300                         ALT-PCB                                          
090400                         PRT-AFTER-1                                      
090500                         CL7INCH-RAD                                      
090600                                                                          
090700     MOVE SPACE                TO CL7INCH-RAD                             
090800     .                                                                    
090900     EJECT                                                                
091000 S03-PRINT-ZEBRA-TERMO7INC  SECTION.                                      
091100                                                                          
091200     MOVE LISTA-IDDISTR   TO KF-ZEBRA-IDDISTR                             
091300     MOVE LISTA-IDKUNDNR  TO KF-ZEBRA-IDKUNDNR                            
091400     MOVE LISTA-IDRAPP    TO KF-ZEBRA-IDRAPP                              
091500     MOVE LISTA-IDKOLLI   TO KF-ZEBRA-IDKOLLI                             
091600     MOVE WS-KILO         TO KF-ZEBRA-KILO                                
091700     MOVE WS-HEKTO        TO KF-ZEBRA-HEKTO                               
091800                                                                          
091900     IF NDC-JP                                                            
092000       MOVE LISTA-ADRESS-1   TO KF-ZEBRA-ADRESS-1                         
092100       MOVE LISTA-ADRESS-2   TO KF-ZEBRA-ADRESS-2                         
092200       MOVE LISTA-ADRESS-4   TO KF-ZEBRA-ADRESS-3                         
092300       MOVE LISTA-ADRESS-3   TO KF-ZEBRA-ADRESS-4                         
092400       MOVE LISTA-ADRESS-5   TO KF-ZEBRA-ADRESS-5                         
092500     ELSE                                                                 
092600       MOVE LISTA-ADRESS-1   TO KF-ZEBRA-ADRESS-1                         
092700       MOVE LISTA-ADRESS-2   TO KF-ZEBRA-ADRESS-2                         
092800       MOVE LISTA-ADRESS-3   TO KF-ZEBRA-ADRESS-3                         
092900       MOVE LISTA-ADRESS-4   TO KF-ZEBRA-ADRESS-4                         
093000       MOVE LISTA-ADRESS-5   TO KF-ZEBRA-ADRESS-5                         
093100     END-IF                                                               
093200                                                                          
093300     MOVE LISTA-ADAVS-1   TO KF-ZEBRA-SHIPPER-COMPANY                     
093400     MOVE LISTA-ADAVS-2   TO KF-ZEBRA-SHIPPER-NAME                        
093500     MOVE LISTA-ADAVS-3   TO KF-ZEBRA-SHIPPER-STREET                      
093600     MOVE LISTA-ADAVS-4   TO KF-ZEBRA-SHIPPER-CITY                        
093700     MOVE LISTA-ADAVS-5   TO KF-ZEBRA-SHIPPER-COUNTRY                     
093800                                                                          
093900     MOVE SPACE       TO KF-ZEBRA-RAD                                     
094000                                                                          
094100     MOVE KF-ZEBRA-STYR-01 TO KF-ZEBRA-RAD                                
094200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE PRT-IDPRTLST             
094300                         ALT-PCB PRT-NYSIDA-RAD1 KF-ZEBRA-RAD             
094400                                                                          
094500     MOVE KF-ZEBRA-RUB-RETURN   TO KF-ZEBRA-RAD                           
094600     PERFORM S04-SKRIV-ZEBRA-RAD                                          
094700                                                                          
094800     MOVE KF-ZEBRA-RUB-IDDISTR  TO KF-ZEBRA-RAD                           
094900     PERFORM S04-SKRIV-ZEBRA-RAD                                          
095000                                                                          
095100     MOVE KF-ZEBRA-RUB-IDKUND   TO KF-ZEBRA-RAD                           
095200     PERFORM S04-SKRIV-ZEBRA-RAD                                          
095300                                                                          
095400     MOVE KF-ZEBRA-RUB-IDRAPP   TO KF-ZEBRA-RAD                           
095500     PERFORM S04-SKRIV-ZEBRA-RAD                                          
095600                                                                          
095700     MOVE KF-ZEBRA-RUB-IDKOLLI  TO KF-ZEBRA-RAD                           
095800     PERFORM S04-SKRIV-ZEBRA-RAD                                          
095900                                                                          
096000     MOVE KF-ZEBRA-RUB-VKORDBTO TO KF-ZEBRA-RAD                           
096100     PERFORM S04-SKRIV-ZEBRA-RAD                                          
096200                                                                          
096300     MOVE KF-ZEBRA-RUB-ADRESS   TO KF-ZEBRA-RAD                           
096400     PERFORM S04-SKRIV-ZEBRA-RAD                                          
096500                                                                          
096600     MOVE KF-ZEBRA-RUB-SHIPPER  TO KF-ZEBRA-RAD                           
096700     PERFORM S04-SKRIV-ZEBRA-RAD                                          
096800                                                                          
096900*DATA-FIELDS                                                              
097000     MOVE KF-ZEBRA-DATA-IDDISTR   TO KF-ZEBRA-RAD                         
097100     PERFORM S04-SKRIV-ZEBRA-RAD                                          
097200                                                                          
097300     MOVE KF-ZEBRA-DATA-IDKUNDNR  TO KF-ZEBRA-RAD                         
097400     PERFORM S04-SKRIV-ZEBRA-RAD                                          
097500                                                                          
097600     MOVE KF-ZEBRA-DATA-IDRAPP    TO KF-ZEBRA-RAD                         
097700     PERFORM S04-SKRIV-ZEBRA-RAD                                          
097800                                                                          
097900     MOVE KF-ZEBRA-DATA-IDKOLLI   TO KF-ZEBRA-RAD                         
098000     PERFORM S04-SKRIV-ZEBRA-RAD                                          
098100                                                                          
098200     MOVE KF-ZEBRA-DATA-WEIGHT    TO KF-ZEBRA-RAD                         
098300     PERFORM S04-SKRIV-ZEBRA-RAD                                          
098400                                                                          
098500     MOVE KF-ZEBRA-DATA-ADRESS-1  TO KF-ZEBRA-RAD                         
098600     PERFORM S04-SKRIV-ZEBRA-RAD                                          
098700                                                                          
098800     MOVE KF-ZEBRA-DATA-ADRESS-2  TO KF-ZEBRA-RAD                         
098900     PERFORM S04-SKRIV-ZEBRA-RAD                                          
099000                                                                          
099100     MOVE KF-ZEBRA-DATA-ADRESS-3  TO KF-ZEBRA-RAD                         
099200     PERFORM S04-SKRIV-ZEBRA-RAD                                          
099300                                                                          
099400     MOVE KF-ZEBRA-DATA-ADRESS-4  TO KF-ZEBRA-RAD                         
099500     PERFORM S04-SKRIV-ZEBRA-RAD                                          
099600                                                                          
099700     MOVE KF-ZEBRA-DATA-ADRESS-5  TO KF-ZEBRA-RAD                         
099800     PERFORM S04-SKRIV-ZEBRA-RAD                                          
099900                                                                          
100000     MOVE KF-ZEBRA-DATA-SHIPPER-COMPANY  TO KF-ZEBRA-RAD                  
100100     PERFORM S04-SKRIV-ZEBRA-RAD                                          
100200                                                                          
100300     MOVE KF-ZEBRA-DATA-SHIPPER-NAME     TO KF-ZEBRA-RAD                  
100400     PERFORM S04-SKRIV-ZEBRA-RAD                                          
100500                                                                          
100600     MOVE KF-ZEBRA-DATA-SHIPPER-STREET   TO KF-ZEBRA-RAD                  
100700     PERFORM S04-SKRIV-ZEBRA-RAD                                          
100800                                                                          
100900     MOVE KF-ZEBRA-DATA-SHIPPER-CITY     TO KF-ZEBRA-RAD                  
101000     PERFORM S04-SKRIV-ZEBRA-RAD                                          
101100                                                                          
101200     MOVE KF-ZEBRA-DATA-SHIPPER-COUNTRY  TO KF-ZEBRA-RAD                  
101300     PERFORM S04-SKRIV-ZEBRA-RAD                                          
101400                                                                          
101500                                                                          
101600     MOVE KF-ZEBRA-STYR-03 TO KF-ZEBRA-RAD                                
101700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE PRT-IDPRTLST             
101800                         ALT-PCB PRT-AFTER-1 KF-ZEBRA-RAD                 
101900                                                                          
102000     .                                                                    
102100     EJECT                                                                
102200 S04-SKRIV-ZEBRA-RAD SECTION.                                             
102300                                                                          
102400     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
102500                         PRT-WRITE                                        
102600                         PRT-IDPRTLST                                     
102700                         ALT-PCB                                          
102800                         PRT-AFTER-1                                      
102900                         KF-ZEBRA-RAD                                     
103000                                                                          
103100     MOVE SPACE                TO KF-ZEBRA-RAD                            
103200     .                                                                    
103300     EJECT                                                                
103400* --- IMS SEKTIONER ---                                                   
103500     SKIP3                                                                
103600 IMS-GET-MSG SECTION.                                                     
103700                                                                          
103800     MOVE '  QC' TO GODK-STATUSKODER                                      
103900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
104000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     SKIP3                                                                
104400 IMS-GU-WDGX4101 SECTION.                                                 
104500                                                                          
104600     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
104700          DELIMITED BY SIZE INTO SSA1                                     
104800     MOVE '  GE' TO GODK-STATUSKODER                                      
104900     CALL CBLTDLI USING GU 4101-PCB DLI-IO-WDGX4101 SSA1                  
105000     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300     EJECT                                                                
105400 IMS-GNP-WDGX4102 SECTION.                                                
105500                                                                          
105600     STRING  'WDGX4102(KY4102  >=' W-WDGX4102-MIN-X                       
105700                     '&KY4102  <=' W-WDGX4102-MAX-X ')'                   
105800              DELIMITED BY SIZE INTO SSA1                                 
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GNP 4101-PCB DLI-IO-WDGX4102 SSA1                 
106100     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP3                                                                
106500 IMS-GU-WDB201 SECTION.                                                   
106600                                                                          
106700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
106800          DELIMITED BY SIZE INTO SSA1                                     
106900     MOVE '  GE' TO GODK-STATUSKODER                                      
107000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
107100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSKONTROLL                                           
107300     .                                                                    
107400     EJECT                                                                
107500 IMS-GU-WDB301 SECTION.                                                   
107600                                                                          
107700     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
107800                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
107900          DELIMITED BY SIZE INTO SSA1                                     
108000     MOVE '  GE' TO GODK-STATUSKODER                                      
108100     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
108200     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
108300     PERFORM IMS-STATUSKONTROLL                                           
108400     .                                                                    
108500     EJECT                                                                
108600 IMS-STATUSKONTROLL SECTION.                                              
108700                                                                          
108800     SET STATUS-IX TO 1                                                   
108900     SEARCH GODK-STATUS                                                   
109000       AT END                                                             
109100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
109200         DELIMITED BY SIZE INTO FELTEXT                                   
109300         CALL FELLOG                                                      
109400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
109500         CONTINUE                                                         
109600     END-SEARCH                                                           
109700     .                                                                    
