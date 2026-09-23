000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0050400.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   APRIL 75.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*     FUNKTION.   TP-PROGRAM FÖR PARTS MENYHANTERING.                     
000900                                                                          
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 DATA DIVISION.                                                           
001300     EJECT                                                                
001400 WORKING-STORAGE SECTION.                                                 
001500*    -- CHECKED BY WY2000                                                 
001600                                                                          
001700 77    IDPGM             PIC X(8)    VALUE 'W0050400'.                    
001800 77    W-COMPILED        PIC X(16)   VALUE SPACE.                         
001900 77    FELTEXT           PIC X(80)   VALUE SPACE.                         
002000 77    JA                PIC X       VALUE 'J'.                           
002100 77    NEJ               PIC X       VALUE 'N'.                           
002200 77    INDX              PIC S9(9)   VALUE ZERO COMP SYNC.                
002300 77    W-KEY             PIC 9(8)    VALUE ZERO.                          
002400 77    P-TO-P-SW         PIC X       VALUE 'N'.                           
002500   88    P-TO-P-OK                   VALUE 'J'.                           
002600                                                                          
002700                                                                          
002800 01  W-TIME              PIC 9(7).                                        
002900 01  FILLER REDEFINES W-TIME.                                             
003000   05  FILLER            PIC 99.                                          
003100   05  W-TIME-MM         PIC 99.                                          
003200   05  FILLER            PIC 999.                                         
003300                                                                          
003400                                                                          
003500 01  W-IDUSER.                                                            
003600   03  W-IDUSER-POS-1-2.                                                  
003700     05  W-IDUSER-POS-1  PIC X(1)    VALUE SPACE.                         
003800     05  FILLER          PIC X(1)    VALUE SPACE.                         
003900   03  FILLER            PIC X(6)    VALUE SPACE.                         
004000                                                                          
004100                                                                          
004200 01  W-MODNAMN.                                                           
004300   03    FILLER          PIC X(5)    VALUE 'W0O50'.                       
004400   03    W-MODN-POS-6    PIC X(1)    VALUE '4'.                           
004500   03    W-MODN-POS-7-8  PIC X(2)    VALUE '01'.                          
004600                                                                          
004700 01  W-VIMSID.                                                            
004800   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004900   03  FILLER                PIC X(4)    VALUE SPACE.                     
005000                                                                          
005100                                                                          
005200 01    DYNAMISKA-SUBPROGRAM.                                              
005300   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
005400   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
005500   03    VIMSID          PIC X(8)    VALUE 'VIMSID  '.                    
005600     EJECT                                                                
005700 01    W-PROG-TO-PROG-SW.                                                 
005800   03    W-P-TO-P-SPAR.                                                   
005900     05    FILLER        PIC X(4)    VALUE SPACE.                         
006000     05    W-P-TO-P-S1   PIC X(999)  VALUE SPACE.                         
006100                                                                          
006200   03    W-P-TO-P-05031.                                                  
006300     05    FILLER        PIC S9(4)   VALUE +17     COMP SYNC.             
006400     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
006500     05    FILLER        PIC X(13)   VALUE 'W0T503  05041'.               
006600                                                                          
006700   03    W-P-TO-P-05032.                                                  
006800     05    FILLER        PIC S9(4)   VALUE +17     COMP SYNC.             
006900     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
007000     05    FILLER        PIC X(13)   VALUE 'W0T503  05042'.               
007100                                                                          
007200   03    W-P-TO-P-98711.                                                  
007300     05    FILLER        PIC S9(4)   VALUE +26     COMP SYNC.             
007400     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
007500     05    W-98711-TRAN  PIC X(13)   VALUE SPACE.                         
007600     05    W-98711-KEY.                                                   
007700       07    W-98711-1   PIC X(1).                                        
007800       07    W-98711-2   PIC X(1).                                        
007900       07    W-98711-3   PIC X(1).                                        
008000       07    W-98711-4   PIC X(1).                                        
008100       07    W-98711-5   PIC X(1).                                        
008200       07    W-98711-6   PIC X(1).                                        
008300       07    W-98711-7   PIC X(1).                                        
008400       07    W-98711-8   PIC X(1).                                        
008500       07    W-98711-9   PIC X(1).                                        
008600                                                                          
008700   03    W-P-TO-P-99401.                                                  
008800     05    FILLER        PIC S9(4)   VALUE +13     COMP SYNC.             
008900     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
009000     05    FILLER        PIC X(9)    VALUE 'ACSI     '.                   
009100                                                                          
009200   03    W-P-TO-P-99411.                                                  
009300     05    FILLER        PIC S9(4)   VALUE +129    COMP SYNC.             
009400     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
009500     05    FILLER        PIC X(14)   VALUE 'A32214  PF11S '.              
009600     05    FILLER        PIC X(1)    VALUE ' '.                           
009700     05    W-99411-ID    PIC X(5)    VALUE SPACE.                         
009800     05    W-99411-ARTNR PIC X(8)    VALUE SPACE.                         
009900     05    FILLER        PIC X(68)   VALUE SPACE.                         
010000     05    FILLER        PIC X(5)    VALUE 'RS'.                          
010100     05    W-99411-KEY1.                                                  
010200       07  W-99411-IDTRA PIC X(4)    VALUE SPACE.                         
010300       07  W-99411-KDMFS PIC X(1)    VALUE SPACE.                         
010400       07  FILLER        PIC X(3)    VALUE SPACE.                         
010500     05    W-99411-KEY2  PIC X(8)    VALUE SPACE.                         
010600     05    W-99411-KEY3  PIC X(8)    VALUE SPACE.                         
010700                                                                          
010800*  03    W-P-TO-P-99461.                                                  
010900*    05    - COPY PUI050     - PRE 9946-                                  
011000                                                                          
011100*  03    W-P-TO-P-99471.                                                  
011200*    05    - COPY PUI028     - PRE 9947-                                  
011300                                                                          
011400*  03    W-P-TO-P-99481.                                                  
011500*    05    - COPY PUI080     - PRE 9948-                                  
011600                                                                          
011700   03    W-P-TO-P-99701.                                                  
011800     05    FILLER        PIC S9(4)   VALUE +117    COMP SYNC.             
011900     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
012000     05    FILLER        PIC X(15)   VALUE 'PGP66000      S'.             
012100     05    FILLER        PIC X(23)   VALUE SPACE.                         
012200     05    P-SW-USER-KDP PIC X(8)    VALUE SPACE.                         
012300     05    FILLER        PIC X(6)    VALUE 'RS    '.                      
012400     05    FILLER        PIC X(61)   VALUE SPACE.                         
012500                                                                          
012600   03    W-P-TO-P-99711.                                                  
012700     05    FILLER        PIC S9(4)   VALUE +13     COMP SYNC.             
012800     05    FILLER        PIC X(2)    VALUE LOW-VALUE.                     
012900     05    FILLER        PIC X(9)    VALUE 'PGP66000 '.                   
013000     EJECT                                                                
013100 01      TABELLER.                                                        
013200*                                    ALLA MENYER I SEKVENS                
013300*                                    FÖRSTA 2 GÅNGEN FÖR PF7              
013400*                                    SISTA = 999 FÖR PF8 SLUT             
013500  02     MENY-TABELL.                                                     
013600   03    FILLER          PIC X(5)    VALUE '   01'.                       
013700   03    FILLER          PIC X(5)    VALUE '   01'.                       
013800   03    FILLER          PIC X(5)    VALUE '00000'.                       
013900   03    FILLER          PIC X(5)    VALUE '0100A'.                       
014000   03    FILLER          PIC X(5)    VALUE '0500M'.                       
014100   03    FILLER          PIC X(5)    VALUE '0600P'.                       
014200   03    FILLER          PIC X(5)    VALUE '0700S'.                       
014300   03    FILLER          PIC X(5)    VALUE '0800V'.                       
014400   03    FILLER          PIC X(5)    VALUE '10010'.                       
014500   03    FILLER          PIC X(5)    VALUE '1101A'.                       
014600   03    FILLER          PIC X(5)    VALUE '1201D'.                       
014700   03    FILLER          PIC X(5)    VALUE '1501M'.                       
014800   03    FILLER          PIC X(5)    VALUE '2002A'.                       
014900   03    FILLER          PIC X(5)    VALUE '2142B'.                       
015000   03    FILLER          PIC X(5)    VALUE '2302G'.                       
015100   03    FILLER          PIC X(5)    VALUE '2342H'.                       
015200   03    FILLER          PIC X(5)    VALUE '2382I'.                       
015300   03    FILLER          PIC X(5)    VALUE '2402J'.                       
015400   03    FILLER          PIC X(5)    VALUE '2442K'.                       
015500   03    FILLER          PIC X(5)    VALUE '30030'.                       
015600   03    FILLER          PIC X(5)    VALUE '3103A'.                       
015700   03    FILLER          PIC X(5)    VALUE '3143B'.                       
015800   03    FILLER          PIC X(5)    VALUE '3203D'.                       
015900   03    FILLER          PIC X(5)    VALUE '3303G'.                       
016000   03    FILLER          PIC X(5)    VALUE '40040'.                       
016100   03    FILLER          PIC X(5)    VALUE '4104A'.                       
016200   03    FILLER          PIC X(5)    VALUE '4204D'.                       
016300   03    FILLER          PIC X(5)    VALUE '4244E'.                       
016400   03    FILLER          PIC X(5)    VALUE '4284F'.                       
016500   03    FILLER          PIC X(5)    VALUE '4304G'.                       
016600   03    FILLER          PIC X(5)    VALUE '4344H'.                       
016700   03    FILLER          PIC X(5)    VALUE '4384I'.                       
016800   03    FILLER          PIC X(5)    VALUE '4404J'.                       
016900   03    FILLER          PIC X(5)    VALUE '4504M'.                       
017000   03    FILLER          PIC X(5)    VALUE '4544N'.                       
017100   03    FILLER          PIC X(5)    VALUE '4584O'.                       
017200   03    FILLER          PIC X(5)    VALUE '4604P'.                       
017300   03    FILLER          PIC X(5)    VALUE '4654R'.                       
017400   03    FILLER          PIC X(5)    VALUE '4704S'.                       
017500   03    FILLER          PIC X(5)    VALUE '4744T'.                       
017600   03    FILLER          PIC X(5)    VALUE '4904Y'.                       
017700   03    FILLER          PIC X(5)    VALUE '5005A'.                       
017800   03    FILLER          PIC X(5)    VALUE '5165B'.                       
017900   03    FILLER          PIC X(5)    VALUE '5205D'.                       
018000   03    FILLER          PIC X(5)    VALUE '5285F'.                       
018100   03    FILLER          PIC X(5)    VALUE '5305G'.                       
018200   03    FILLER          PIC X(5)    VALUE '6006A'.                       
018300   03    FILLER          PIC X(5)    VALUE '6146B'.                       
018400   03    FILLER          PIC X(5)    VALUE '6186C'.                       
018500   03    FILLER          PIC X(5)    VALUE '6206D'.                       
018600   03    FILLER          PIC X(5)    VALUE '6306G'.                       
018700   03    FILLER          PIC X(5)    VALUE '6346H'.                       
018800   03    FILLER          PIC X(5)    VALUE '9009A'.                       
018900   03    FILLER          PIC X(5)    VALUE '92092'.                       
019000   03    FILLER          PIC X(5)    VALUE '93093'.                       
019100   03    FILLER          PIC X(5)    VALUE '99099'.                       
019200   03    FILLER          PIC X(5)    VALUE '99999'.                       
019300 02    MENY-TAB REDEFINES MENY-TABELL OCCURS 57                           
019400         ASCENDING KEY IS TAB-MENY-NR INDEXED BY MENY-IX.                 
019500   03    TAB-MENY-NR     PIC X(3).                                        
019600   03    TAB-MENYN-7-8   PIC X(2).                                        
019700     EJECT                                                                
019800  02     MODNAMN-TABELL.                                                  
019900   03    FILLER          PIC X(13)   VALUE '0107NW0O10701'.               
020000   03    FILLER          PIC X(13)   VALUE '0440NW0O44001'.               
020100   03    FILLER          PIC X(13)   VALUE '0441NW0O44101'.               
020200   03    FILLER          PIC X(13)   VALUE '0442NW0O44201'.               
020300   03    FILLER          PIC X(13)   VALUE '0443NW0O44301'.               
020400   03    FILLER          PIC X(13)   VALUE '0444NW0O44401'.               
020500   03    FILLER          PIC X(13)   VALUE '0445NW0O44501'.               
020600   03    FILLER          PIC X(13)   VALUE '0446NW0O44601'.               
020700   03    FILLER          PIC X(13)   VALUE '0447NW0O44701'.               
020800   03    FILLER          PIC X(13)   VALUE '0448NW0O44801'.               
020900   03    FILLER          PIC X(13)   VALUE '0449NW0O44901'.               
021000   03    FILLER          PIC X(13)   VALUE '0501NWSLOGON '.               
021100   03    FILLER          PIC X(13)   VALUE '0502NWSPW    '.               
021200   03    FILLER          PIC X(13)   VALUE '0503 W0O50301'.               
021300   03    FILLER          PIC X(13)   VALUE '0504NWMENY   '.               
021400   03    FILLER          PIC X(13)   VALUE '0508 W0O50801'.               
021500   03    FILLER          PIC X(13)   VALUE '0509 W0O50901'.               
021600   03    FILLER          PIC X(13)   VALUE '0510 W0O51001'.               
021700   03    FILLER          PIC X(13)   VALUE '0511 W0O51101'.               
021800   03    FILLER          PIC X(13)   VALUE '0513NW0O51301'.               
021900   03    FILLER          PIC X(13)   VALUE '0514 W0O51401'.               
022000   03    FILLER          PIC X(13)   VALUE '0515 W0O515N1'.               
022100   03    FILLER          PIC X(13)   VALUE '0521NW0O521N1'.               
022200   03    FILLER          PIC X(13)   VALUE '0541 W0O54101'.               
022300   03    FILLER          PIC X(13)   VALUE '0551NW0O55101'.               
022400   03    FILLER          PIC X(13)   VALUE '0552NW0O55201'.               
022500   03    FILLER          PIC X(13)   VALUE '0553NW0O55301'.               
022600   03    FILLER          PIC X(13)   VALUE '0554NW0O55401'.               
022700   03    FILLER          PIC X(13)   VALUE '0555NW0O55501'.               
022800   03    FILLER          PIC X(13)   VALUE '0601NW0O60101'.               
022900   03    FILLER          PIC X(13)   VALUE '0604 W0O60401'.               
023000   03    FILLER          PIC X(13)   VALUE '0605 W0O60501'.               
023100   03    FILLER          PIC X(13)   VALUE '0606 W0O60601'.               
023200   03    FILLER          PIC X(13)   VALUE '0607NW0O60701'.               
023300   03    FILLER          PIC X(13)   VALUE '0608NW0O60801'.               
023400   03    FILLER          PIC X(13)   VALUE '0621 W0O62101'.               
023500   03    FILLER          PIC X(13)   VALUE '0622 W0O62201'.               
023600   03    FILLER          PIC X(13)   VALUE '0623NW0O62301'.               
023700   03    FILLER          PIC X(13)   VALUE '0691SW0O69101'.               
023800   03    FILLER          PIC X(13)   VALUE '0701 W0O70101'.               
023900   03    FILLER          PIC X(13)   VALUE '0702 W0O70201'.               
024000   03    FILLER          PIC X(13)   VALUE '0703 W0O70301'.               
024100   03    FILLER          PIC X(13)   VALUE '0704 W0O70401'.               
024200   03    FILLER          PIC X(13)   VALUE '0705 W0O70501'.               
024300   03    FILLER          PIC X(13)   VALUE '0706 W0O70601'.               
024400   03    FILLER          PIC X(13)   VALUE '0707 W0O70701'.               
024500   03    FILLER          PIC X(13)   VALUE '0708 W0O70801'.               
024600   03    FILLER          PIC X(13)   VALUE '0709 W0O70901'.               
024700   03    FILLER          PIC X(13)   VALUE '0791 W0O79101'.               
024800   03    FILLER          PIC X(13)   VALUE '0801NW0O80101'.               
024900   03    FILLER          PIC X(13)   VALUE '0803NW0O80301'.               
025000   03    FILLER          PIC X(13)   VALUE '0804NW0O80401'.               
025100   03    FILLER          PIC X(13)   VALUE '0805NW0O80501'.               
025200   03    FILLER          PIC X(13)   VALUE '0807NW0O80701'.               
025300   03    FILLER          PIC X(13)   VALUE '0808NW0O80801'.               
025400   03    FILLER          PIC X(13)   VALUE '0809NW0O80901'.               
025500   03    FILLER          PIC X(13)   VALUE '0811 W0O81101'.               
025600   03    FILLER          PIC X(13)   VALUE '0812 W0O81201'.               
025700   03    FILLER          PIC X(13)   VALUE '0813NW0O81301'.               
025800   03    FILLER          PIC X(13)   VALUE '0814 W0O81401'.               
025900   03    FILLER          PIC X(13)   VALUE '0815NW0O815N1'.               
026000   03    FILLER          PIC X(13)   VALUE '0816NW0O816N1'.               
026100   03    FILLER          PIC X(13)   VALUE '0832 W0O83201'.               
026200   03    FILLER          PIC X(13)   VALUE '1101NW1O10101'.               
026300   03    FILLER          PIC X(13)   VALUE '1102NW1O10201'.               
026400   03    FILLER          PIC X(13)   VALUE '1103NW1O10301'.               
026500   03    FILLER          PIC X(13)   VALUE '1105NW1O105N1'.               
026600   03    FILLER          PIC X(13)   VALUE '1107NW1O10701'.               
026700   03    FILLER          PIC X(13)   VALUE '1108NW1O10801'.               
026800   03    FILLER          PIC X(13)   VALUE '1111NW1O11101'.               
026900   03    FILLER          PIC X(13)   VALUE '1112NW1O11201'.               
027000   03    FILLER          PIC X(13)   VALUE '1113NW1O11301'.               
027100   03    FILLER          PIC X(13)   VALUE '1114NW1O11401'.               
027200   03    FILLER          PIC X(13)   VALUE '1115NW1O11501'.               
027300   03    FILLER          PIC X(13)   VALUE '1116NW1O116N1'.               
027400   03    FILLER          PIC X(13)   VALUE '1117NW1O11701'.               
027500   03    FILLER          PIC X(13)   VALUE '1118NW1O11801'.               
027600   03    FILLER          PIC X(13)   VALUE '1121NW1O12101'.               
027700   03    FILLER          PIC X(13)   VALUE '1122NW1O12201'.               
027800   03    FILLER          PIC X(13)   VALUE '1131NW1O13101'.               
027900   03    FILLER          PIC X(13)   VALUE '1132NW1O132N1'.               
028000   03    FILLER          PIC X(13)   VALUE '1142NW1O14201'.               
028100   03    FILLER          PIC X(13)   VALUE '1151NW1O15101'.               
028200   03    FILLER          PIC X(13)   VALUE '1153NW1O15301'.               
028300   03    FILLER          PIC X(13)   VALUE '1154NW1O15401'.               
028400   03    FILLER          PIC X(13)   VALUE '1155 W1O15501'.               
028500   03    FILLER          PIC X(13)   VALUE '1161NW1O16101'.               
028600   03    FILLER          PIC X(13)   VALUE '1202 W1O20201'.               
028700   03    FILLER          PIC X(13)   VALUE '1211NW1O21101'.               
028800   03    FILLER          PIC X(13)   VALUE '1212NW1O21201'.               
028900   03    FILLER          PIC X(13)   VALUE '1213NW1O213N1'.               
029000   03    FILLER          PIC X(13)   VALUE '1214NW1O21401'.               
029100   03    FILLER          PIC X(13)   VALUE '1215NW1O21501'.               
029200   03    FILLER          PIC X(13)   VALUE '1221NW1O22101'.               
029300   03    FILLER          PIC X(13)   VALUE '1222NW1O22201'.               
029400   03    FILLER          PIC X(13)   VALUE '1231 W1O23101'.               
029500   03    FILLER          PIC X(13)   VALUE '1501NW1O50101'.               
029600   03    FILLER          PIC X(13)   VALUE '1502NW1O50201'.               
029700   03    FILLER          PIC X(13)   VALUE '1505NW1O50501'.               
029800   03    FILLER          PIC X(13)   VALUE '1508NW1O50801'.               
029900   03    FILLER          PIC X(13)   VALUE '1511NW1O51101'.               
030000   03    FILLER          PIC X(13)   VALUE '1512NW1O51201'.               
030100   03    FILLER          PIC X(13)   VALUE '1513NW1O51301'.               
030200   03    FILLER          PIC X(13)   VALUE '1514NW1O51401'.               
030300   03    FILLER          PIC X(13)   VALUE '1515 W1O51501'.               
030400   03    FILLER          PIC X(13)   VALUE '1518NW1O51801'.               
030500   03    FILLER          PIC X(13)   VALUE '1519NW1O51901'.               
030600   03    FILLER          PIC X(13)   VALUE '1522 W1O52201'.               
030700   03    FILLER          PIC X(13)   VALUE '1523 W1O52301'.               
030800   03    FILLER          PIC X(13)   VALUE '1524 W1O52401'.               
030900   03    FILLER          PIC X(13)   VALUE '1525 W1O52501'.               
031000   03    FILLER          PIC X(13)   VALUE '1526NW1O52601'.               
031100   03    FILLER          PIC X(13)   VALUE '1531NW1O53101'.               
031200   03    FILLER          PIC X(13)   VALUE '1532 W1O53201'.               
031300   03    FILLER          PIC X(13)   VALUE '1533 W1O53301'.               
031400   03    FILLER          PIC X(13)   VALUE '1541NW1O54101'.               
031500   03    FILLER          PIC X(13)   VALUE '1542NW1O54201'.               
031600   03    FILLER          PIC X(13)   VALUE '1543NW1O54301'.               
031700   03    FILLER          PIC X(13)   VALUE '1551NW1O55101'.               
031800   03    FILLER          PIC X(13)   VALUE '1552NW1O55201'.               
031900   03    FILLER          PIC X(13)   VALUE '1561NW1O56101'.               
032000   03    FILLER          PIC X(13)   VALUE '2101NW2O10101'.               
032100   03    FILLER          PIC X(13)   VALUE '2102NW2O10201'.               
032200   03    FILLER          PIC X(13)   VALUE '2103 W2O10301'.               
032300   03    FILLER          PIC X(13)   VALUE '2104 W2O10401'.               
032400   03    FILLER          PIC X(13)   VALUE '2105 W2O10501'.               
032500   03    FILLER          PIC X(13)   VALUE '2106 W2O10601'.               
032600   03    FILLER          PIC X(13)   VALUE '2107NW2O10701'.               
032700   03    FILLER          PIC X(13)   VALUE '2108 W2O10801'.               
032800   03    FILLER          PIC X(13)   VALUE '2109NW2O10901'.               
032900   03    FILLER          PIC X(13)   VALUE '2111NW2O11101'.               
033000   03    FILLER          PIC X(13)   VALUE '2112NW2O11201'.               
033100   03    FILLER          PIC X(13)   VALUE '2113NW2O113N1'.               
033200   03    FILLER          PIC X(13)   VALUE '2114NW2O114N1'.               
033300   03    FILLER          PIC X(13)   VALUE '2115NW2O11501'.               
033400   03    FILLER          PIC X(13)   VALUE '2116NW2O116N1'.               
033500   03    FILLER          PIC X(13)   VALUE '2117NW2O117N1'.               
033600   03    FILLER          PIC X(13)   VALUE '2118NW2O118N1'.               
033700   03    FILLER          PIC X(13)   VALUE '2119NW2O119N1'.               
033800   03    FILLER          PIC X(13)   VALUE '2121 W2O12101'.               
033900   03    FILLER          PIC X(13)   VALUE '2122 W2O12201'.               
034000   03    FILLER          PIC X(13)   VALUE '2123 W2O12301'.               
034100   03    FILLER          PIC X(13)   VALUE '2124 W2O124N1'.               
034200   03    FILLER          PIC X(13)   VALUE '2125NW2O12501'.               
034300   03    FILLER          PIC X(13)   VALUE '2126NW2O12601'.               
034400   03    FILLER          PIC X(13)   VALUE '2127NW2O12701'.               
034500   03    FILLER          PIC X(13)   VALUE '2128NW2O128N1'.               
034600   03    FILLER          PIC X(13)   VALUE '2129NW2O12901'.               
034700   03    FILLER          PIC X(13)   VALUE '2131NW2O131N1'.               
034800   03    FILLER          PIC X(13)   VALUE '2132NW2O132N1'.               
034900   03    FILLER          PIC X(13)   VALUE '2133NW2O13301'.               
035000   03    FILLER          PIC X(13)   VALUE '2134NW2O13401'.               
035100   03    FILLER          PIC X(13)   VALUE '2136NW2O13601'.               
035200   03    FILLER          PIC X(13)   VALUE '2137NW2O13701'.               
035300   03    FILLER          PIC X(13)   VALUE '2138NW2O13801'.               
035400   03    FILLER          PIC X(13)   VALUE '2139NW2O139N1'.               
035500   03    FILLER          PIC X(13)   VALUE '2141NW2O14101'.               
035600   03    FILLER          PIC X(13)   VALUE '2142NW2O14201'.               
035700   03    FILLER          PIC X(13)   VALUE '2143NW2O143N1'.               
035800   03    FILLER          PIC X(13)   VALUE '2144 W2O14401'.               
035900   03    FILLER          PIC X(13)   VALUE '2145NW2O145N1'.               
036000   03    FILLER          PIC X(13)   VALUE '2146 W2O146N1'.               
036100   03    FILLER          PIC X(13)   VALUE '2147 W2O147N1'.               
036200   03    FILLER          PIC X(13)   VALUE '2148 W2O14801'.               
036300   03    FILLER          PIC X(13)   VALUE '2149NW2O149N1'.               
036400   03    FILLER          PIC X(13)   VALUE '2151NW2O15101'.               
036500   03    FILLER          PIC X(13)   VALUE '2152NW2O15201'.               
036600   03    FILLER          PIC X(13)   VALUE '2153NW2O153N1'.               
036700   03    FILLER          PIC X(13)   VALUE '2158NW2O158N1'.               
036800   03    FILLER          PIC X(13)   VALUE '2171NW2O17101'.               
036900   03    FILLER          PIC X(13)   VALUE '2172 W2O17201'.               
037000   03    FILLER          PIC X(13)   VALUE '2301NW2O30101'.               
037100   03    FILLER          PIC X(13)   VALUE '2302NW2O30201'.               
037200   03    FILLER          PIC X(13)   VALUE '2303NW2O30301'.               
037300   03    FILLER          PIC X(13)   VALUE '2311NW2O311N1'.               
037400   03    FILLER          PIC X(13)   VALUE '2312NW2O312N1'.               
037500   03    FILLER          PIC X(13)   VALUE '2313NW2O313N1'.               
037600   03    FILLER          PIC X(13)   VALUE '2314NW2O314N1'.               
037700   03    FILLER          PIC X(13)   VALUE '2315NW2O315N1'.               
037800   03    FILLER          PIC X(13)   VALUE '2316NW2O316N1'.               
037900   03    FILLER          PIC X(13)   VALUE '2317NW2O317N1'.               
038000   03    FILLER          PIC X(13)   VALUE '2321 W2O32101'.               
038100   03    FILLER          PIC X(13)   VALUE '2322 W2O322N1'.               
038200   03    FILLER          PIC X(13)   VALUE '2323 W2O323N1'.               
038300   03    FILLER          PIC X(13)   VALUE '2324 W2O32401'.               
038400   03    FILLER          PIC X(13)   VALUE '2325 W2O325N1'.               
038500   03    FILLER          PIC X(13)   VALUE '2332 W2O332N1'.               
038600   03    FILLER          PIC X(13)   VALUE '2333 W2O333N1'.               
038700   03    FILLER          PIC X(13)   VALUE '2334 W2O334N1'.               
038800   03    FILLER          PIC X(13)   VALUE '2335 W2O335N1'.               
038900   03    FILLER          PIC X(13)   VALUE '2336 W2O336N1'.               
039000   03    FILLER          PIC X(13)   VALUE '2337 W2O337N1'.               
039100   03    FILLER          PIC X(13)   VALUE '2338 W2O338N1'.               
039200   03    FILLER          PIC X(13)   VALUE '2339 W2O339N1'.               
039300   03    FILLER          PIC X(13)   VALUE '2341NW2O341N1'.               
039400   03    FILLER          PIC X(13)   VALUE '2342NW2O34201'.               
039500   03    FILLER          PIC X(13)   VALUE '2343NW2O343N1'.               
039600   03    FILLER          PIC X(13)   VALUE '2344 W2O344N1'.               
039700   03    FILLER          PIC X(13)   VALUE '2346NW2O34601'.               
039800   03    FILLER          PIC X(13)   VALUE '2347NW2O34701'.               
039900   03    FILLER          PIC X(13)   VALUE '2348NW2O348N1'.               
040000   03    FILLER          PIC X(13)   VALUE '2349NW2O349N1'.               
040100   03    FILLER          PIC X(13)   VALUE '2351NW2O351N1'.               
040200   03    FILLER          PIC X(13)   VALUE '2352NW2O352N1'.               
040300   03    FILLER          PIC X(13)   VALUE '2353NW2O353N1'.               
040400   03    FILLER          PIC X(13)   VALUE '2354NW2O354N1'.               
040500   03    FILLER          PIC X(13)   VALUE '2355NW2O355N1'.               
040600   03    FILLER          PIC X(13)   VALUE '2356NW2O356N1'.               
040700   03    FILLER          PIC X(13)   VALUE '2357NW2O357N1'.               
040800   03    FILLER          PIC X(13)   VALUE '2358NW2O358N1'.               
040900   03    FILLER          PIC X(13)   VALUE '2359NW2O359N1'.               
041000   03    FILLER          PIC X(13)   VALUE '2361NW2O361N1'.               
041100   03    FILLER          PIC X(13)   VALUE '2362NW2O362N1'.               
041200   03    FILLER          PIC X(13)   VALUE '2363NW2O363N1'.               
041300   03    FILLER          PIC X(13)   VALUE '2364NW2O364N1'.               
041400   03    FILLER          PIC X(13)   VALUE '2365NW2O365N1'.               
041500   03    FILLER          PIC X(13)   VALUE '2366NW2O366N1'.               
041600   03    FILLER          PIC X(13)   VALUE '2367NW2O367N1'.               
041700   03    FILLER          PIC X(13)   VALUE '2368NW2O368N1'.               
041800   03    FILLER          PIC X(13)   VALUE '2371NW2O371N1'.               
041900   03    FILLER          PIC X(13)   VALUE '2372NW2O372N1'.               
042000   03    FILLER          PIC X(13)   VALUE '2373NW2O373N1'.               
042100   03    FILLER          PIC X(13)   VALUE '2377NW2O377N1'.               
042200   03    FILLER          PIC X(13)   VALUE '2381NW2O381N1'.               
042300   03    FILLER          PIC X(13)   VALUE '2382NW2O382N1'.               
042400   03    FILLER          PIC X(13)   VALUE '2391NW2O391N1'.               
042500   03    FILLER          PIC X(13)   VALUE '2392NW2O392N1'.               
042600   03    FILLER          PIC X(13)   VALUE '2393NW2O393N1'.               
042700   03    FILLER          PIC X(13)   VALUE '2401NW2O401N1'.               
042800   03    FILLER          PIC X(13)   VALUE '2402NW2O402N1'.               
042900   03    FILLER          PIC X(13)   VALUE '2403NW2O403N1'.               
043000   03    FILLER          PIC X(13)   VALUE '2404NW2O404N1'.               
043100   03    FILLER          PIC X(13)   VALUE '2405NW2O405N1'.               
043200   03    FILLER          PIC X(13)   VALUE '2406NW2O406N1'.               
043300   03    FILLER          PIC X(13)   VALUE '2407NW2O407N1'.               
043400   03    FILLER          PIC X(13)   VALUE '2421NW2O421N1'.               
043500   03    FILLER          PIC X(13)   VALUE '2422NW2O422N1'.               
043600   03    FILLER          PIC X(13)   VALUE '2423NW2O423N1'.               
043700   03    FILLER          PIC X(13)   VALUE '2425NW2O425N1'.               
043800   03    FILLER          PIC X(13)   VALUE '2428NW2O428N1'.               
043900   03    FILLER          PIC X(13)   VALUE '2431NW2O431N1'.               
044000   03    FILLER          PIC X(13)   VALUE '2432NW2O432N1'.               
044100   03    FILLER          PIC X(13)   VALUE '2433NW2O433N1'.               
044200   03    FILLER          PIC X(13)   VALUE '2439NW2O439N1'.               
044300   03    FILLER          PIC X(13)   VALUE '2441NW2O441N1'.               
044400   03    FILLER          PIC X(13)   VALUE '2442NW2O442N1'.               
044500   03    FILLER          PIC X(13)   VALUE '2446NW2O446N1'.               
044600   03    FILLER          PIC X(13)   VALUE '2447NW2O447N1'.               
044700   03    FILLER          PIC X(13)   VALUE '2452NW2O452N1'.               
044800   03    FILLER          PIC X(13)   VALUE '2455NW2O455N1'.               
044810   03    FILLER          PIC X(13)   VALUE '2456NW2O456N1'.               
044900   03    FILLER          PIC X(13)   VALUE '2457NW2O457N1'.               
045000   03    FILLER          PIC X(13)   VALUE '2459NW2O459N1'.               
045100   03    FILLER          PIC X(13)   VALUE '2471NW2O471N1'.               
045200   03    FILLER          PIC X(13)   VALUE '2472NW2O472N1'.               
045300   03    FILLER          PIC X(13)   VALUE '3101 W3O10101'.               
045400   03    FILLER          PIC X(13)   VALUE '3103NW3O103N1'.               
045500   03    FILLER          PIC X(13)   VALUE '3111NW3O11101'.               
045600   03    FILLER          PIC X(13)   VALUE '3112NW3O11201'.               
045700   03    FILLER          PIC X(13)   VALUE '3121NW3O121N1'.               
045800   03    FILLER          PIC X(13)   VALUE '3123NW3O123N1'.               
045900   03    FILLER          PIC X(13)   VALUE '3124NW3O124N1'.               
046000   03    FILLER          PIC X(13)   VALUE '3125NW3O125N1'.               
046100   03    FILLER          PIC X(13)   VALUE '3126NW3O126N1'.               
046200   03    FILLER          PIC X(13)   VALUE '3127NW3O127N1'.               
046300   03    FILLER          PIC X(13)   VALUE '3151NW3O151N1'.               
046400   03    FILLER          PIC X(13)   VALUE '3152NW3O15201'.               
046500   03    FILLER          PIC X(13)   VALUE '3153NW3O153N1'.               
046600   03    FILLER          PIC X(13)   VALUE '3154NW3O154N1'.               
046700   03    FILLER          PIC X(13)   VALUE '3161NW3O16101'.               
046800   03    FILLER          PIC X(13)   VALUE '3162NW3O16201'.               
046900   03    FILLER          PIC X(13)   VALUE '3164NW3O164N1'.               
047000   03    FILLER          PIC X(13)   VALUE '3165NW3O165N1'.               
047100   03    FILLER          PIC X(13)   VALUE '3166NW3O166N1'.               
047200   03    FILLER          PIC X(13)   VALUE '3167NW3O167N1'.               
047300   03    FILLER          PIC X(13)   VALUE '3168NW3O168N1'.               
047400   03    FILLER          PIC X(13)   VALUE '3169NW3O169N1'.               
047500   03    FILLER          PIC X(13)   VALUE '3171NW3O17101'.               
047600   03    FILLER          PIC X(13)   VALUE '3172NW3O17201'.               
047700   03    FILLER          PIC X(13)   VALUE '3173NW3O173N1'.               
047800   03    FILLER          PIC X(13)   VALUE '3176NW3O17601'.               
047900   03    FILLER          PIC X(13)   VALUE '3177 W3O177N1'.               
048000   03    FILLER          PIC X(13)   VALUE '3182NW3O18201'.               
048100   03    FILLER          PIC X(13)   VALUE '3183NW3O18301'.               
048200   03    FILLER          PIC X(13)   VALUE '3184 W3O184N1'.               
048300   03    FILLER          PIC X(13)   VALUE '3185 W3O185N1'.               
048400   03    FILLER          PIC X(13)   VALUE '3201 W3O20101'.               
048500   03    FILLER          PIC X(13)   VALUE '3202 W3O20201'.               
048600   03    FILLER          PIC X(13)   VALUE '3203 W3O20301'.               
048700   03    FILLER          PIC X(13)   VALUE '3204 W3O20401'.               
048800   03    FILLER          PIC X(13)   VALUE '3211 W3O21101'.               
048900   03    FILLER          PIC X(13)   VALUE '3212 W3O21201'.               
049000   03    FILLER          PIC X(13)   VALUE '3213 W3O21301'.               
049100   03    FILLER          PIC X(13)   VALUE '3221NW3O22101'.               
049200   03    FILLER          PIC X(13)   VALUE '3304NW3O30401'.               
049300   03    FILLER          PIC X(13)   VALUE '3305NW3O305N1'.               
049400   03    FILLER          PIC X(13)   VALUE '3306NW3O306N1'.               
049500   03    FILLER          PIC X(13)   VALUE '3307NW3O307N1'.               
049600   03    FILLER          PIC X(13)   VALUE '3311NW3O31101'.               
049700   03    FILLER          PIC X(13)   VALUE '3312NW3O31201'.               
049800   03    FILLER          PIC X(13)   VALUE '3313NW3O31301'.               
049900   03    FILLER          PIC X(13)   VALUE '3314NW3O31401'.               
050000   03    FILLER          PIC X(13)   VALUE '3315NW3O31501'.               
050100   03    FILLER          PIC X(13)   VALUE '3316NW3O31601'.               
050200   03    FILLER          PIC X(13)   VALUE '3317NW3O31701'.               
050300   03    FILLER          PIC X(13)   VALUE '3318 W3O31801'.               
050400   03    FILLER          PIC X(13)   VALUE '3321NW3O321N1'.               
050500   03    FILLER          PIC X(13)   VALUE '3322NW3O32201'.               
050600   03    FILLER          PIC X(13)   VALUE '3331NW3O33101'.               
050700   03    FILLER          PIC X(13)   VALUE '4101NW4O10101'.               
050800   03    FILLER          PIC X(13)   VALUE '4102NW4O10201'.               
050900   03    FILLER          PIC X(13)   VALUE '4103NW4O10301'.               
051000   03    FILLER          PIC X(13)   VALUE '4104NW4O10401'.               
051100   03    FILLER          PIC X(13)   VALUE '4105NW4O105N1'.               
051200   03    FILLER          PIC X(13)   VALUE '4106NW4O106N1'.               
051300   03    FILLER          PIC X(13)   VALUE '4107NW4O107N1'.               
051400   03    FILLER          PIC X(13)   VALUE '4108NW4O10801'.               
051500   03    FILLER          PIC X(13)   VALUE '4111NW4O11101'.               
051600   03    FILLER          PIC X(13)   VALUE '4115NW4O11501'.               
051700   03    FILLER          PIC X(13)   VALUE '4116NW4O11601'.               
051800   03    FILLER          PIC X(13)   VALUE '4121NW4O12101'.               
051900   03    FILLER          PIC X(13)   VALUE '4122NW4O12201'.               
052000   03    FILLER          PIC X(13)   VALUE '4123NW4O12301'.               
052100   03    FILLER          PIC X(13)   VALUE '4131NW4O13101'.               
052200   03    FILLER          PIC X(13)   VALUE '4141NW4O141N1'.               
052300   03    FILLER          PIC X(13)   VALUE '4201NW4O20101'.               
052400   03    FILLER          PIC X(13)   VALUE '4202NW4O20201'.               
052500   03    FILLER          PIC X(13)   VALUE '4203NW4O20301'.               
052600   03    FILLER          PIC X(13)   VALUE '4204NW4O20401'.               
052700   03    FILLER          PIC X(13)   VALUE '4205NW4O20501'.               
052800   03    FILLER          PIC X(13)   VALUE '4211NW4O21101'.               
052900   03    FILLER          PIC X(13)   VALUE '4213NW4O21301'.               
053000   03    FILLER          PIC X(13)   VALUE '4214NW4O21401'.               
053100   03    FILLER          PIC X(13)   VALUE '4221NW4O22101'.               
053200   03    FILLER          PIC X(13)   VALUE '4223NW4O22301'.               
053300   03    FILLER          PIC X(13)   VALUE '4224NW4O22401'.               
053400   03    FILLER          PIC X(13)   VALUE '4225NW4O225N1'.               
053500   03    FILLER          PIC X(13)   VALUE '4226NW4O226N1'.               
053600   03    FILLER          PIC X(13)   VALUE '4227NW4O227N1'.               
053700   03    FILLER          PIC X(13)   VALUE '4228NW4O228N1'.               
053800   03    FILLER          PIC X(13)   VALUE '4231NW4O23101'.               
053900   03    FILLER          PIC X(13)   VALUE '4233NW4O23301'.               
054000   03    FILLER          PIC X(13)   VALUE '4241NW4O24101'.               
054100   03    FILLER          PIC X(13)   VALUE '4243NW4O24301'.               
054200   03    FILLER          PIC X(13)   VALUE '4244NW4O24401'.               
054300   03    FILLER          PIC X(13)   VALUE '4245NW4O24501'.               
054400   03    FILLER          PIC X(13)   VALUE '4246NW4O24601'.               
054500   03    FILLER          PIC X(13)   VALUE '4261NW4O26101'.               
054600   03    FILLER          PIC X(13)   VALUE '4262NW4O26201'.               
054700   03    FILLER          PIC X(13)   VALUE '4264NW4O26401'.               
054800   03    FILLER          PIC X(13)   VALUE '4265NW4O26501'.               
054900   03    FILLER          PIC X(13)   VALUE '4266NW4O26601'.               
055000   03    FILLER          PIC X(13)   VALUE '4267NW4O26701'.               
055100   03    FILLER          PIC X(13)   VALUE '4268NW4O26801'.               
055200   03    FILLER          PIC X(13)   VALUE '4271NW4O271N1'.               
055300   03    FILLER          PIC X(13)   VALUE '4281NW4O28101'.               
055400   03    FILLER          PIC X(13)   VALUE '4282NW4O28201'.               
055500   03    FILLER          PIC X(13)   VALUE '4283NW4O28301'.               
055600   03    FILLER          PIC X(13)   VALUE '4284NW4O28401'.               
055700   03    FILLER          PIC X(13)   VALUE '4301NW4O30101'.               
055800   03    FILLER          PIC X(13)   VALUE '4302NW4O30201'.               
055900   03    FILLER          PIC X(13)   VALUE '4303NW4O30301'.               
056000   03    FILLER          PIC X(13)   VALUE '4304 W4O30401'.               
056100   03    FILLER          PIC X(13)   VALUE '4305NW4O30501'.               
056200   03    FILLER          PIC X(13)   VALUE '4311NW4O31101'.               
056300   03    FILLER          PIC X(13)   VALUE '4312NW4O31201'.               
056400   03    FILLER          PIC X(13)   VALUE '4313NW4O31301'.               
056500   03    FILLER          PIC X(13)   VALUE '4314NW4O31401'.               
056600   03    FILLER          PIC X(13)   VALUE '4315NW4O31501'.               
056700   03    FILLER          PIC X(13)   VALUE '4316NW4O31601'.               
056800   03    FILLER          PIC X(13)   VALUE '4318NW4O31801'.               
056900   03    FILLER          PIC X(13)   VALUE '4321NW4O32101'.               
057000   03    FILLER          PIC X(13)   VALUE '4322NW4O32201'.               
057100   03    FILLER          PIC X(13)   VALUE '4323NW4O32301'.               
057200   03    FILLER          PIC X(13)   VALUE '4324NW4O32401'.               
057300   03    FILLER          PIC X(13)   VALUE '4325NW4O32501'.               
057400   03    FILLER          PIC X(13)   VALUE '4326NW4O326N1'.               
057500   03    FILLER          PIC X(13)   VALUE '4331NW4O33101'.               
057600   03    FILLER          PIC X(13)   VALUE '4332NW4O33201'.               
057700   03    FILLER          PIC X(13)   VALUE '4333NW4O33301'.               
057800   03    FILLER          PIC X(13)   VALUE '4334NW4O33401'.               
057900   03    FILLER          PIC X(13)   VALUE '4336NW4O33601'.               
058000   03    FILLER          PIC X(13)   VALUE '4338 W4O33801'.               
058100   03    FILLER          PIC X(13)   VALUE '4341NW4O34101'.               
058200   03    FILLER          PIC X(13)   VALUE '4342NW4O34201'.               
058300   03    FILLER          PIC X(13)   VALUE '4343NW4O34301'.               
058400   03    FILLER          PIC X(13)   VALUE '4344NW4O34401'.               
058500   03    FILLER          PIC X(13)   VALUE '4345NW4O34501'.               
058600   03    FILLER          PIC X(13)   VALUE '4346NW4O346N1'.               
058700   03    FILLER          PIC X(13)   VALUE '4347NW4O347N1'.               
058800   03    FILLER          PIC X(13)   VALUE '4348NW4O34801'.               
058900   03    FILLER          PIC X(13)   VALUE '4351NW4O35101'.               
059000   03    FILLER          PIC X(13)   VALUE '4352NW4O35201'.               
059100   03    FILLER          PIC X(13)   VALUE '4353NW4O35301'.               
059200   03    FILLER          PIC X(13)   VALUE '4354 W4O35401'.               
059300   03    FILLER          PIC X(13)   VALUE '4356NW4O35601'.               
059400   03    FILLER          PIC X(13)   VALUE '4357NW4O35701'.               
059500   03    FILLER          PIC X(13)   VALUE '4358NW4O35801'.               
059600   03    FILLER          PIC X(13)   VALUE '4359NW4O35901'.               
059700   03    FILLER          PIC X(13)   VALUE '4361NW4O36101'.               
059800   03    FILLER          PIC X(13)   VALUE '4362NW4O36201'.               
059900   03    FILLER          PIC X(13)   VALUE '4363NW4O36301'.               
060000   03    FILLER          PIC X(13)   VALUE '4364NW4O36401'.               
060100   03    FILLER          PIC X(13)   VALUE '4365NW4O36501'.               
060200   03    FILLER          PIC X(13)   VALUE '4366NW4O36601'.               
060300   03    FILLER          PIC X(13)   VALUE '4367NW4O36701'.               
060400   03    FILLER          PIC X(13)   VALUE '4368NW4O36801'.               
060500   03    FILLER          PIC X(13)   VALUE '4369NW4O36901'.               
060600   03    FILLER          PIC X(13)   VALUE '4381NW4O38101'.               
060700   03    FILLER          PIC X(13)   VALUE '4382NW4O38201'.               
060800   03    FILLER          PIC X(13)   VALUE '4383NW4O38301'.               
060900   03    FILLER          PIC X(13)   VALUE '4384NW4O38401'.               
061000   03    FILLER          PIC X(13)   VALUE '4385NW4O38501'.               
061100   03    FILLER          PIC X(13)   VALUE '4386NW4O38601'.               
061200   03    FILLER          PIC X(13)   VALUE '4387NW4O38701'.               
061300   03    FILLER          PIC X(13)   VALUE '4388NW4O38801'.               
061400   03    FILLER          PIC X(13)   VALUE '4389NW4O38901'.               
061500   03    FILLER          PIC X(13)   VALUE '4401 W4O401N1'.               
061510   03    FILLER          PIC X(13)   VALUE '4402 W4O402N1'.               
061600   03    FILLER          PIC X(13)   VALUE '4403 W4O403N1'.               
061700   03    FILLER          PIC X(13)   VALUE '4404 W4O404N1'.               
061800   03    FILLER          PIC X(13)   VALUE '4405 W4O405N1'.               
061900   03    FILLER          PIC X(13)   VALUE '4406 W4O406N1'.               
062000   03    FILLER          PIC X(13)   VALUE '4407 W4O407N1'.               
062100   03    FILLER          PIC X(13)   VALUE '4408 W4O408N1'.               
062200   03    FILLER          PIC X(13)   VALUE '4409 W4O409N1'.               
062300   03    FILLER          PIC X(13)   VALUE '4411 W4O411N1'.               
062400   03    FILLER          PIC X(13)   VALUE '4412 W4O412N1'.               
062500   03    FILLER          PIC X(13)   VALUE '4413 W4O413N1'.               
062600   03    FILLER          PIC X(13)   VALUE '4414 W4O414N1'.               
062700   03    FILLER          PIC X(13)   VALUE '4415 W4O415N1'.               
062800   03    FILLER          PIC X(13)   VALUE '4416 W4O416N1'.               
062900   03    FILLER          PIC X(13)   VALUE '4417 W4O417N1'.               
063000   03    FILLER          PIC X(13)   VALUE '4418 W4O418N1'.               
063100   03    FILLER          PIC X(13)   VALUE '4421 W4O421N1'.               
063200   03    FILLER          PIC X(13)   VALUE '4422 W4O422N1'.               
063300   03    FILLER          PIC X(13)   VALUE '4431 W4O431N1'.               
063400   03    FILLER          PIC X(13)   VALUE '4441 W4O441N1'.               
063500   03    FILLER          PIC X(13)   VALUE '4456NW4O456N1'.               
063510   03    FILLER          PIC X(13)   VALUE '4457NW4O457N1'.               
063600   03    FILLER          PIC X(13)   VALUE '4482 W4O482N1'.               
063700   03    FILLER          PIC X(13)   VALUE '4483 W4O483N1'.               
063800   03    FILLER          PIC X(13)   VALUE '4484 W4O484N1'.               
063900   03    FILLER          PIC X(13)   VALUE '4501NW4O50101'.               
064000   03    FILLER          PIC X(13)   VALUE '4502NW4O50201'.               
064100   03    FILLER          PIC X(13)   VALUE '4503NW4O50301'.               
064200   03    FILLER          PIC X(13)   VALUE '4504NW4O50401'.               
064300   03    FILLER          PIC X(13)   VALUE '4507NW4O50701'.               
064400   03    FILLER          PIC X(13)   VALUE '4508NW4O50801'.               
064500   03    FILLER          PIC X(13)   VALUE '4509NW4O50901'.               
064600   03    FILLER          PIC X(13)   VALUE '4511NW4O51101'.               
064700   03    FILLER          PIC X(13)   VALUE '4512NW4O51201'.               
064800   03    FILLER          PIC X(13)   VALUE '4513NW4O51301'.               
064900   03    FILLER          PIC X(13)   VALUE '4514NW4O51401'.               
065000   03    FILLER          PIC X(13)   VALUE '4515NW4O51501'.               
065100   03    FILLER          PIC X(13)   VALUE '4517NW4O51701'.               
065200   03    FILLER          PIC X(13)   VALUE '4521NW4O52101'.               
065300   03    FILLER          PIC X(13)   VALUE '4538 W4O53801'.               
065400   03    FILLER          PIC X(13)   VALUE '4544NW4O54401'.               
065500   03    FILLER          PIC X(13)   VALUE '4551NW4O55101'.               
065600   03    FILLER          PIC X(13)   VALUE '4561NW4O56101'.               
065700   03    FILLER          PIC X(13)   VALUE '4571NW4O57101'.               
065800   03    FILLER          PIC X(13)   VALUE '4572NW4O57201'.               
065900   03    FILLER          PIC X(13)   VALUE '4573NW4O57301'.               
066000   03    FILLER          PIC X(13)   VALUE '4574NW4O57401'.               
066100   03    FILLER          PIC X(13)   VALUE '4575NW4O57501'.               
066200   03    FILLER          PIC X(13)   VALUE '4576 W4O576N1'.               
066300   03    FILLER          PIC X(13)   VALUE '4578 W4O578N1'.               
066400   03    FILLER          PIC X(13)   VALUE '4581NW4O58101'.               
066500   03    FILLER          PIC X(13)   VALUE '4582NW4O58201'.               
066600   03    FILLER          PIC X(13)   VALUE '4611 W4O61101'.               
066700   03    FILLER          PIC X(13)   VALUE '4622NW4O622N1'.               
066800   03    FILLER          PIC X(13)   VALUE '4623NW4O623N1'.               
066900   03    FILLER          PIC X(13)   VALUE '4624NW4O624N1'.               
067000   03    FILLER          PIC X(13)   VALUE '4625NW4O625N1'.               
067100   03    FILLER          PIC X(13)   VALUE '4651NW4O65101'.               
067200   03    FILLER          PIC X(13)   VALUE '4652NW4O65201'.               
067300   03    FILLER          PIC X(13)   VALUE '4653NW4O65301'.               
067400   03    FILLER          PIC X(13)   VALUE '4654NW4O65401'.               
067500   03    FILLER          PIC X(13)   VALUE '4661NW4O66101'.               
067600   03    FILLER          PIC X(13)   VALUE '4663NW4O66301'.               
067700   03    FILLER          PIC X(13)   VALUE '4664NW4O66401'.               
067800   03    FILLER          PIC X(13)   VALUE '4666NW4O66601'.               
067900   03    FILLER          PIC X(13)   VALUE '4667NW4O66701'.               
068000   03    FILLER          PIC X(13)   VALUE '4668NW4O66801'.               
068100   03    FILLER          PIC X(13)   VALUE '4669 W4O669N1'.               
068200   03    FILLER          PIC X(13)   VALUE '4671NW4O67101'.               
068300   03    FILLER          PIC X(13)   VALUE '4672NW4O67201'.               
068400   03    FILLER          PIC X(13)   VALUE '4673NW4O67301'.               
068500   03    FILLER          PIC X(13)   VALUE '4701NW4O70101'.               
068600   03    FILLER          PIC X(13)   VALUE '4702NW4O70201'.               
068700   03    FILLER          PIC X(13)   VALUE '4703NW4O70301'.               
068800   03    FILLER          PIC X(13)   VALUE '4704NW4O70401'.               
068900   03    FILLER          PIC X(13)   VALUE '4705NW4O70501'.               
069000   03    FILLER          PIC X(13)   VALUE '4706NW4O70601'.               
069100   03    FILLER          PIC X(13)   VALUE '4707NW4O707N1'.               
069200   03    FILLER          PIC X(13)   VALUE '4708NW4O708N1'.               
069300   03    FILLER          PIC X(13)   VALUE '4709NW4O709N1'.               
069400   03    FILLER          PIC X(13)   VALUE '4711NW4O71101'.               
069500   03    FILLER          PIC X(13)   VALUE '4712NW4O71201'.               
069600   03    FILLER          PIC X(13)   VALUE '4713NW4O71301'.               
069700   03    FILLER          PIC X(13)   VALUE '4714NW4O71401'.               
069800   03    FILLER          PIC X(13)   VALUE '4715NW4O71501'.               
069900   03    FILLER          PIC X(13)   VALUE '4716NW4O71601'.               
070000   03    FILLER          PIC X(13)   VALUE '4717NW4O717N1'.               
070100   03    FILLER          PIC X(13)   VALUE '4718NW4O718N1'.               
070200   03    FILLER          PIC X(13)   VALUE '4719NW4O719N1'.               
070300   03    FILLER          PIC X(13)   VALUE '4721NW4O72101'.               
070400   03    FILLER          PIC X(13)   VALUE '4722NW4O72201'.               
070500   03    FILLER          PIC X(13)   VALUE '4723NW4O72301'.               
070600   03    FILLER          PIC X(13)   VALUE '4724NW4O72401'.               
070700   03    FILLER          PIC X(13)   VALUE '4725NW4O72501'.               
070800   03    FILLER          PIC X(13)   VALUE '4726NW4O72601'.               
070900   03    FILLER          PIC X(13)   VALUE '4727NW4O72701'.               
071000   03    FILLER          PIC X(13)   VALUE '4731NW4O73101'.               
071100   03    FILLER          PIC X(13)   VALUE '4732NW4O73201'.               
071200   03    FILLER          PIC X(13)   VALUE '4733NW4O73301'.               
071300   03    FILLER          PIC X(13)   VALUE '4734NW4O73401'.               
071400   03    FILLER          PIC X(13)   VALUE '4735NW4O73501'.               
071500   03    FILLER          PIC X(13)   VALUE '4736NW4O73601'.               
071600   03    FILLER          PIC X(13)   VALUE '4737NW4O73701'.               
071700   03    FILLER          PIC X(13)   VALUE '4738NW4O73801'.               
071800   03    FILLER          PIC X(13)   VALUE '4739NW4O739N1'.               
071900   03    FILLER          PIC X(13)   VALUE '4741NW4O74101'.               
072000   03    FILLER          PIC X(13)   VALUE '4742NW4O74201'.               
072100   03    FILLER          PIC X(13)   VALUE '4743NW4O74301'.               
072200   03    FILLER          PIC X(13)   VALUE '4744NW4O74401'.               
072300   03    FILLER          PIC X(13)   VALUE '4745NW4O745N1'.               
072400   03    FILLER          PIC X(13)   VALUE '4748NW4O748N1'.               
072500   03    FILLER          PIC X(13)   VALUE '4751NW4O751N1'.               
072600   03    FILLER          PIC X(13)   VALUE '4752NW4O752N1'.               
072700   03    FILLER          PIC X(13)   VALUE '4753NW4O753N1'.               
072800   03    FILLER          PIC X(13)   VALUE '4754NW4O754N1'.               
072900   03    FILLER          PIC X(13)   VALUE '4901NW4O90101'.               
073000   03    FILLER          PIC X(13)   VALUE '4902NW4O90201'.               
073100   03    FILLER          PIC X(13)   VALUE '4903NW4O90301'.               
073200   03    FILLER          PIC X(13)   VALUE '4904NW4O90401'.               
073300   03    FILLER          PIC X(13)   VALUE '5101NW5O10101'.               
073400   03    FILLER          PIC X(13)   VALUE '5102 W5O10201'.               
073500   03    FILLER          PIC X(13)   VALUE '5103 W5O10301'.               
073600   03    FILLER          PIC X(13)   VALUE '5104 W5O10401'.               
073700   03    FILLER          PIC X(13)   VALUE '5105NW5O10501'.               
073800   03    FILLER          PIC X(13)   VALUE '5106NW5O10601'.               
073900   03    FILLER          PIC X(13)   VALUE '5107NW5O10701'.               
074000   03    FILLER          PIC X(13)   VALUE '5108NW5O10801'.               
074100   03    FILLER          PIC X(13)   VALUE '5109NW5O10901'.               
074200   03    FILLER          PIC X(13)   VALUE '5111 W5O11101'.               
074300   03    FILLER          PIC X(13)   VALUE '5112 W5O11201'.               
074400   03    FILLER          PIC X(13)   VALUE '5113 W5O11301'.               
074500   03    FILLER          PIC X(13)   VALUE '5114 W5O11401'.               
074600   03    FILLER          PIC X(13)   VALUE '5115 W5O11501'.               
074700   03    FILLER          PIC X(13)   VALUE '5119 W5O11901'.               
074800   03    FILLER          PIC X(13)   VALUE '5122 W5O12201'.               
074900   03    FILLER          PIC X(13)   VALUE '5123NW5O123N1'.               
075000   03    FILLER          PIC X(13)   VALUE '5125NW5O125N1'.               
075100   03    FILLER          PIC X(13)   VALUE '5131NW5O13101'.               
075200   03    FILLER          PIC X(13)   VALUE '5132 W5O13201'.               
075300   03    FILLER          PIC X(13)   VALUE '5133NW5O133N1'.               
075400   03    FILLER          PIC X(13)   VALUE '5134NW5O134N1'.               
075500   03    FILLER          PIC X(13)   VALUE '5135NW5O135N1'.               
075510   03    FILLER          PIC X(13)   VALUE '5136NW5O136N1'.               
075520   03    FILLER          PIC X(13)   VALUE '5137NW5O137N1'.               
075600   03    FILLER          PIC X(13)   VALUE '5141 W5O14101'.               
075700   03    FILLER          PIC X(13)   VALUE '5142 W5O14201'.               
075800   03    FILLER          PIC X(13)   VALUE '5143 W5O14301'.               
075900   03    FILLER          PIC X(13)   VALUE '5144 W5O14401'.               
076000   03    FILLER          PIC X(13)   VALUE '5145NW5O145N1'.               
076100   03    FILLER          PIC X(13)   VALUE '5151NW5O15101'.               
076200   03    FILLER          PIC X(13)   VALUE '5161NW5O161N1'.               
076300   03    FILLER          PIC X(13)   VALUE '5162NW5O162N1'.               
076400   03    FILLER          PIC X(13)   VALUE '5163NW5O163N1'.               
076500   03    FILLER          PIC X(13)   VALUE '5166NW5O166N1'.               
076600   03    FILLER          PIC X(13)   VALUE '5201 W5O20101'.               
076700   03    FILLER          PIC X(13)   VALUE '5202 W5O20201'.               
076800   03    FILLER          PIC X(13)   VALUE '5203NW5O203N1'.               
076900   03    FILLER          PIC X(13)   VALUE '5205NW5O205N1'.               
077000   03    FILLER          PIC X(13)   VALUE '5206NW5O206N1'.               
077100   03    FILLER          PIC X(13)   VALUE '5207NW5O207N1'.               
077200   03    FILLER          PIC X(13)   VALUE '5211 W5O211N1'.               
077300   03    FILLER          PIC X(13)   VALUE '5212 W5O212N1'.               
077400   03    FILLER          PIC X(13)   VALUE '5213 W5O213N1'.               
077500   03    FILLER          PIC X(13)   VALUE '5214 W5O214N1'.               
077600   03    FILLER          PIC X(13)   VALUE '5215 W5O215N1'.               
077700   03    FILLER          PIC X(13)   VALUE '5216 W5O216N1'.               
077800   03    FILLER          PIC X(13)   VALUE '5217 W5O217N1'.               
077900   03    FILLER          PIC X(13)   VALUE '5218 W5O218N1'.               
078000   03    FILLER          PIC X(13)   VALUE '5219 W5O219N1'.               
078100   03    FILLER          PIC X(13)   VALUE '5221 W5O221N1'.               
078200   03    FILLER          PIC X(13)   VALUE '5222NW5O222N1'.               
078300   03    FILLER          PIC X(13)   VALUE '5223NW5O223N1'.               
078400   03    FILLER          PIC X(13)   VALUE '5285NW5O285N1'.               
078500   03    FILLER          PIC X(13)   VALUE '5286NW5O286N1'.               
078600   03    FILLER          PIC X(13)   VALUE '5302NW5O30201'.               
078700   03    FILLER          PIC X(13)   VALUE '5303NW5O30301'.               
078800   03    FILLER          PIC X(13)   VALUE '5304NW5O30401'.               
078900   03    FILLER          PIC X(13)   VALUE '5305NW5O30501'.               
079000   03    FILLER          PIC X(13)   VALUE '5306NW5O30601'.               
079100   03    FILLER          PIC X(13)   VALUE '5307NW5O30701'.               
079200   03    FILLER          PIC X(13)   VALUE '5308NW5O30801'.               
079300   03    FILLER          PIC X(13)   VALUE '5309NW5O30901'.               
079400   03    FILLER          PIC X(13)   VALUE '5311NW5O311N1'.               
079500   03    FILLER          PIC X(13)   VALUE '5312NW5O312N1'.               
079600   03    FILLER          PIC X(13)   VALUE '5321NW5O321N1'.               
079700   03    FILLER          PIC X(13)   VALUE '6101NW6O10101'.               
079800   03    FILLER          PIC X(13)   VALUE '6102NW6O10201'.               
079900   03    FILLER          PIC X(13)   VALUE '6103NW6O10301'.               
080000   03    FILLER          PIC X(13)   VALUE '6104NW6O10401'.               
080100   03    FILLER          PIC X(13)   VALUE '6105NW6O10501'.               
080200   03    FILLER          PIC X(13)   VALUE '6106NW6O10601'.               
080300   03    FILLER          PIC X(13)   VALUE '6107NW6O10701'.               
080400   03    FILLER          PIC X(13)   VALUE '6108NW6O10801'.               
080500   03    FILLER          PIC X(13)   VALUE '6109NW6O10901'.               
080600   03    FILLER          PIC X(13)   VALUE '6111NW6O11101'.               
080700   03    FILLER          PIC X(13)   VALUE '6112NW6O11201'.               
080800   03    FILLER          PIC X(13)   VALUE '6113NW6O11301'.               
080900   03    FILLER          PIC X(13)   VALUE '6114NW6O11401'.               
081000   03    FILLER          PIC X(13)   VALUE '6115NW6O11501'.               
081100   03    FILLER          PIC X(13)   VALUE '6116NW6O11601'.               
081200   03    FILLER          PIC X(13)   VALUE '6117NW6O11701'.               
081300   03    FILLER          PIC X(13)   VALUE '6118NW6O11801'.               
081400   03    FILLER          PIC X(13)   VALUE '6119NW6O11901'.               
081500   03    FILLER          PIC X(13)   VALUE '6121NW6O12101'.               
081600   03    FILLER          PIC X(13)   VALUE '6122NW6O12201'.               
081700   03    FILLER          PIC X(13)   VALUE '6123NW6O12301'.               
081800   03    FILLER          PIC X(13)   VALUE '6124NW6O12401'.               
081900   03    FILLER          PIC X(13)   VALUE '6125NW6O12501'.               
082000   03    FILLER          PIC X(13)   VALUE '6126NW6O12601'.               
082100   03    FILLER          PIC X(13)   VALUE '6131NW6O13101'.               
082200   03    FILLER          PIC X(13)   VALUE '6132NW6O13201'.               
082300   03    FILLER          PIC X(13)   VALUE '6133NW6O13301'.               
082400   03    FILLER          PIC X(13)   VALUE '6135NW6O13501'.               
082500   03    FILLER          PIC X(13)   VALUE '6136NW6O13601'.               
082600   03    FILLER          PIC X(13)   VALUE '6137NW6O13701'.               
082700   03    FILLER          PIC X(13)   VALUE '6139NW6O13901'.               
082800   03    FILLER          PIC X(13)   VALUE '6141 W6O14101'.               
082900   03    FILLER          PIC X(13)   VALUE '6142 W6O14201'.               
083000   03    FILLER          PIC X(13)   VALUE '6143NW6O14301'.               
083100   03    FILLER          PIC X(13)   VALUE '6144NW6O14401'.               
083200   03    FILLER          PIC X(13)   VALUE '6145NW6O14501'.               
083300   03    FILLER          PIC X(13)   VALUE '6146NW6O14601'.               
083400   03    FILLER          PIC X(13)   VALUE '6147NW6O14701'.               
083500   03    FILLER          PIC X(13)   VALUE '6148 W6O14801'.               
083600   03    FILLER          PIC X(13)   VALUE '6151 W6O15101'.               
083700   03    FILLER          PIC X(13)   VALUE '6152 W6O15201'.               
083800   03    FILLER          PIC X(13)   VALUE '6153 W6O15301'.               
083900   03    FILLER          PIC X(13)   VALUE '6154 W6O15401'.               
084000   03    FILLER          PIC X(13)   VALUE '6155 W6O15501'.               
084100   03    FILLER          PIC X(13)   VALUE '6157 W6O15701'.               
084200   03    FILLER          PIC X(13)   VALUE '6158 W6O15801'.               
084300   03    FILLER          PIC X(13)   VALUE '6161NW6O16101'.               
084400   03    FILLER          PIC X(13)   VALUE '6162NW6O16201'.               
084500   03    FILLER          PIC X(13)   VALUE '6163NW6O16301'.               
084600   03    FILLER          PIC X(13)   VALUE '6164NW6O164N1'.               
084700   03    FILLER          PIC X(13)   VALUE '6165NW6O16501'.               
084800   03    FILLER          PIC X(13)   VALUE '6166NW6O16601'.               
084900   03    FILLER          PIC X(13)   VALUE '6167NW6O16701'.               
085000   03    FILLER          PIC X(13)   VALUE '6168NW6O16801'.               
085100   03    FILLER          PIC X(13)   VALUE '6169NW6O169N1'.               
085200   03    FILLER          PIC X(13)   VALUE '6171 W6O17101'.               
085300   03    FILLER          PIC X(13)   VALUE '6172 W6O17201'.               
085400   03    FILLER          PIC X(13)   VALUE '6173NW6O17301'.               
085500   03    FILLER          PIC X(13)   VALUE '6174 W6O174N1'.               
085600   03    FILLER          PIC X(13)   VALUE '6175 W6O175N1'.               
085700   03    FILLER          PIC X(13)   VALUE '6176 W6O176N1'.               
085800   03    FILLER          PIC X(13)   VALUE '6181 W6O18101'.               
085900   03    FILLER          PIC X(13)   VALUE '6182NW6O18201'.               
086000   03    FILLER          PIC X(13)   VALUE '6183 W6O18301'.               
086100   03    FILLER          PIC X(13)   VALUE '6184 W6O18401'.               
086200   03    FILLER          PIC X(13)   VALUE '6185 W6O18501'.               
086300   03    FILLER          PIC X(13)   VALUE '6186 W6O186N1'.               
086400   03    FILLER          PIC X(13)   VALUE '6201NW6O20101'.               
086500   03    FILLER          PIC X(13)   VALUE '6202NW6O20201'.               
086600   03    FILLER          PIC X(13)   VALUE '6203NW6O20301'.               
086700   03    FILLER          PIC X(13)   VALUE '6204NW6O20401'.               
086800   03    FILLER          PIC X(13)   VALUE '6207 W6O207N1'.               
086900   03    FILLER          PIC X(13)   VALUE '6208NW6O20801'.               
087000   03    FILLER          PIC X(13)   VALUE '6211NW6O21101'.               
087100   03    FILLER          PIC X(13)   VALUE '6212NW6O212N1'.               
087200   03    FILLER          PIC X(13)   VALUE '6213NW6O21301'.               
087300   03    FILLER          PIC X(13)   VALUE '6214NW6O214N1'.               
087400   03    FILLER          PIC X(13)   VALUE '6215NW6O215N1'.               
087500   03    FILLER          PIC X(13)   VALUE '6216NW6O216N1'.               
087600   03    FILLER          PIC X(13)   VALUE '6217NW6O217N1'.               
087700   03    FILLER          PIC X(13)   VALUE '6221NW6O22101'.               
087800   03    FILLER          PIC X(13)   VALUE '6231NW6O23101'.               
087900   03    FILLER          PIC X(13)   VALUE '6301NW6O301N1'.               
088000   03    FILLER          PIC X(13)   VALUE '6302NW6O30201'.               
088100   03    FILLER          PIC X(13)   VALUE '6303NW6O303N1'.               
088200   03    FILLER          PIC X(13)   VALUE '6304NW6O30401'.               
088300   03    FILLER          PIC X(13)   VALUE '6305NW6O305N1'.               
088400   03    FILLER          PIC X(13)   VALUE '6306NW6O30601'.               
088500   03    FILLER          PIC X(13)   VALUE '6307NW6O307N1'.               
088600   03    FILLER          PIC X(13)   VALUE '6308NW6O308N1'.               
088700   03    FILLER          PIC X(13)   VALUE '6309NW6O30901'.               
088800   03    FILLER          PIC X(13)   VALUE '6311NW6O311N1'.               
088900   03    FILLER          PIC X(13)   VALUE '6312NW6O312N1'.               
089000   03    FILLER          PIC X(13)   VALUE '6313NW6O313N1'.               
089100   03    FILLER          PIC X(13)   VALUE '6314NW6O314N1'.               
089200   03    FILLER          PIC X(13)   VALUE '6315NW6O315N1'.               
089300   03    FILLER          PIC X(13)   VALUE '6316NW6O316N1'.               
089400   03    FILLER          PIC X(13)   VALUE '6317NW6O317N1'.               
089500   03    FILLER          PIC X(13)   VALUE '6318NW6O318N1'.               
089600   03    FILLER          PIC X(13)   VALUE '6319NW6O319N1'.               
089700   03    FILLER          PIC X(13)   VALUE '6321NW6O321N1'.               
089800   03    FILLER          PIC X(13)   VALUE '6322NW6O322N1'.               
089900   03    FILLER          PIC X(13)   VALUE '6323NW6O323N1'.               
090000   03    FILLER          PIC X(13)   VALUE '6324NW6O324N1'.               
090100   03    FILLER          PIC X(13)   VALUE '6325NW6O325N1'.               
090200   03    FILLER          PIC X(13)   VALUE '6331NW6O331N1'.               
090300   03    FILLER          PIC X(13)   VALUE '6332NW6O332N1'.               
090400   03    FILLER          PIC X(13)   VALUE '6341NW6O341N1'.               
090500   03    FILLER          PIC X(13)   VALUE '6342NW6O342N1'.               
090600   03    FILLER          PIC X(13)   VALUE '6343NW6O343N1'.               
090700   03    FILLER          PIC X(13)   VALUE '6344NW6O344N1'.               
090800   03    FILLER          PIC X(13)   VALUE '6345NW6O345N1'.               
090900   03    FILLER          PIC X(13)   VALUE '6346NW6O346N1'.               
091000   03    FILLER          PIC X(13)   VALUE '6347NW6O347N1'.               
091100   03    FILLER          PIC X(13)   VALUE '6348NW6O348N1'.               
091200   03    FILLER          PIC X(13)   VALUE '6351NW6O351N1'.               
091300   03    FILLER          PIC X(13)   VALUE '9102NW9O10201'.               
091400   03    FILLER          PIC X(13)   VALUE '9103NW9O10301'.               
091500   03    FILLER          PIC X(13)   VALUE '9111 W9O11101'.               
091600   03    FILLER          PIC X(13)   VALUE '9112NW9O11201'.               
091700   03    FILLER          PIC X(13)   VALUE '9113NW9O113N1'.               
091800   03    FILLER          PIC X(13)   VALUE '9114NW9O11401'.               
091900   03    FILLER          PIC X(13)   VALUE '9115NW9O115N1'.               
092000   03    FILLER          PIC X(13)   VALUE '9121NW9O12101'.               
092100   03    FILLER          PIC X(13)   VALUE '9122NW9O12201'.               
092200   03    FILLER          PIC X(13)   VALUE '9940 ACSI    '.               
092300   03    FILLER          PIC X(13)   VALUE '9941 A32214  '.               
092400   03    FILLER          PIC X(13)   VALUE '9942 A32214  '.               
092500   03    FILLER          PIC X(13)   VALUE '9943 A32214  '.               
092600   03    FILLER          PIC X(13)   VALUE '9945 A32214  '.               
092700   03    FILLER          PIC X(13)   VALUE '9946 A32214  '.               
092800   03    FILLER          PIC X(13)   VALUE '9947 A32214  '.               
092900   03    FILLER          PIC X(13)   VALUE '9948 A32214  '.               
093000   03    FILLER          PIC X(13)   VALUE '9961 STDSTART'.               
093100   03    FILLER          PIC X(13)   VALUE '9971 PGOSTART'.               
093200   03    FILLER          PIC X(13)   VALUE '9998 DVH     '.               
093300   03    FILLER          PIC X(13)   VALUE '9999 DMENY   '.               
093400 02    MODNAMN-TAB REDEFINES MODNAMN-TABELL OCCURS 740                    
093500         ASCENDING KEY IS TAB-IDTRANS INDEXED BY TAB-IX.                  
093600   03    TAB-IDTRANS     PIC X(4).                                        
093700   03    TAB-HOMR        PIC X.                                           
093800   03    TAB-MODNAMN     PIC X(8).                                        
093900     EJECT                                                                
094000******************************************************************        
094100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
094200*                                                                         
094300 01      FILLER                  PIC X(16)   VALUE 'MFS-WS'.              
094400                                                                          
094500 01    MID-W0150401.                                                      
094600   03    MID-MODNAMN-NR.                                                  
094700     05     MID-IDTRANS.                                                  
094800       07   MID-IDTRANS-POS-1-3.                                          
094900         09 MID-IDTRANS-POS-1    PIC X(1).                                
095000         09 MID-IDTRANS-POS-2    PIC X(1).                                
095100         09 MID-IDTRANS-POS-3    PIC X(1).                                
095200       07   MID-IDTRANS-POS-4    PIC X(1).                                
095300     05  MID-KDMFSFOR            PIC X(1).                                
095400                                                                          
095500*                           COPYTEXT A322W012                             
095600   03    MID-98711.                                                       
095700     05    MID-98711-IDTRANS.                                             
095800       07  MID-98711-POS-1       PIC X(1).                                
095900       07  MID-98711-POS-2-4     PIC X(3).                                
096000     05    MID-98711-POS-5       PIC X(1).                                
096100     05    MID-98711-KOMMA       PIC X(1).                                
096200     05    MID-98711-KEY         PIC X(8).                                
096300                                                                          
096400   03    MID-PVINK.                                                       
096500     05    FILLER                PIC X(1).                                
096600     05    MID-PVINK-1           PIC X(8).                                
096700     05    FILLER                PIC X(1).                                
096800     05    MID-PVINK-2           PIC X(8).                                
096900                                                                          
097000 01    MSG-SPAR-2.                                                        
097100   03    MSG-SPAR-KDTRANS.                                                
097200     05    MSG-SPAR-KDTRANS-TAB  OCCURS 8  PIC X(1).                      
097300   03    FILLER REDEFINES MSG-SPAR-KDTRANS.                               
097400     05    MSG-SPAR-KDTRANS1-6   PIC X(6).                                
097500     05    FILLER                PIC X(2).                                
097600   03    MSG-SPAR-1.                                                      
097700     05    MSG-SPAR-DATA OCCURS 50 PIC X.                                 
097800                                                                          
097900 01      FILLER                  PIC X(7)    VALUE 'MID-END'.             
098000     EJECT                                                                
098100*01      -COPY WMSGAREA                                                   
098200     EJECT                                                                
098300   03    MOD-FORMAT          REDEFINES MSG-AREA.                          
098400     05  MOD-IDTRANS         PIC X(4).                                    
098500     05  MOD-IDLANDX2        PIC X(2).                                    
098600     05  MOD-TEMFSFEL        PIC X(40).                                   
098700     05  FILLER              PIC X(1887).                                 
098800     SKIP3                                                                
098900   03    MOD-MENY            REDEFINES MSG-AREA.                          
099000     05  FILLER              PIC X(4).                                    
099100     05  MOD-KDMFSFOR        PIC X(1).                                    
099200     05  MOD-TEMFSINF        PIC X(55).                                   
099300     05  FILLER              PIC X(1873).                                 
099400     EJECT                                                                
099500*01      -COPY WMFSAREA                                                   
099600     EJECT                                                                
099700******************************************************************        
099800*                                                                         
099900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
100000*                                                                         
100100 01  IMS-WS.                                                              
100200   03    FILLER              PIC X(16)   VALUE ' IMS-WS     '.            
100300     SKIP3                                                                
100400   03    STATUS-WS           PIC XX.                                      
100500     88  STATUS-OK                       VALUE '  '.                      
100600     88  SEGMENT-FINNS                   VALUE '  '.                      
100700     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
100800     88  TRANSKOD-FEL                    VALUE 'A1'.                      
100900     88  SECURITY-FEL                    VALUE 'A4'.                      
101000     SKIP3                                                                
101100   03    GODK-STATUSKODER.                                                
101200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
101300     EJECT                                                                
101400*01      -COPY W0003                                                      
101500     EJECT                                                                
101600 LINKAGE SECTION.                                                         
101700*01  -COPY W0009     -PRE MSG-                                            
101800     EJECT                                                                
101900*01  -COPY W0009     -PRE ALT-                                            
102000     SKIP2                                                                
102100*01  -COPY W0009     -PRE ALT0505-                                        
102200     EJECT                                                                
102300 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ALT0505-PCB.                    
102400 MAIN SECTION.                                                            
102500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT0505-PCB.                   
102600                                                                          
102700     MOVE SPACE TO MSG-KDTRANS-1 MSG-IDTRANS-1                            
102800     MOVE ZERO TO MSG-KDMFSFOR-1                                          
102900     PERFORM IMS-GET-MSG                                                  
103000     IF SEGMENT-FINNS                                                     
103100       PERFORM A-INIT-SPARA-INPUT                                         
103200       PERFORM D-PROG-TO-PROG                                             
103300       IF P-TO-P-OK                                                       
103400         CONTINUE                                                         
103500       ELSE                                                               
103600         IF MFS-ENTER                                                     
103700           PERFORM B-SOEK-TRANS-I-TABELL                                  
103800         END-IF                                                           
103900         IF NOT MFS-ENTER                                                 
104000           PERFORM H-SOEK-TRANS-I-MENY                                    
104100         END-IF                                                           
104200         PERFORM G-SEND-INFO                                              
104300         PERFORM IMS-INSERT-MSG                                           
104400       END-IF                                                             
104500     END-IF                                                               
104600     MOVE ZERO TO RETURN-CODE                                             
104700     GOBACK                                                               
104800     .                                                                    
104900     EJECT                                                                
105000 A-INIT-SPARA-INPUT SECTION.                                              
105100                                                                          
105200     MOVE WHEN-COMPILED TO W-COMPILED                                     
105300     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
105400                                                                          
105500     IF MSG-DUBBLA-TRANSKODER                                             
105600       MOVE MSG-AREA    TO MSG-SPAR-2                                     
105700       MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                    
105800       MOVE MSG-IDPFK   TO MFS-IDPFK                                      
105900                                                                          
106000       IF MSG-SPAR-KDTRANS1-6 = 'WHELP2'                                  
106100           OR MSG-SPAR-DATA (1) = 'R'                                     
106200           OR MSG-KDTRANS-2 = 'WINFO   '                                  
106300         MOVE MSG-SPAR-DATA (9)  TO MID-IDTRANS-POS-1                     
106400         MOVE MSG-SPAR-DATA (10) TO MID-IDTRANS-POS-2                     
106500         MOVE MSG-SPAR-DATA (11) TO MID-IDTRANS-POS-3                     
106600         MOVE MSG-SPAR-DATA (12) TO MID-IDTRANS-POS-4                     
106700         IF MID-IDTRANS = '0503'                                          
106800           MOVE ' ' TO MID-IDTRANS-POS-1                                  
106900         END-IF                                                           
107000       ELSE                                                               
107100         MOVE MSG-SPAR-DATA (2)  TO MID-IDTRANS-POS-1                     
107200         MOVE MSG-SPAR-DATA (4)  TO MID-IDTRANS-POS-2                     
107300         MOVE MSG-SPAR-DATA (5)  TO MID-IDTRANS-POS-3                     
107400         MOVE MSG-SPAR-DATA (6)  TO MID-IDTRANS-POS-4                     
107500         IF MID-IDTRANS < '9800'                                          
107600           MOVE '0' TO MID-IDTRANS-POS-4                                  
107700           MOVE 'X' TO MFS-IDPFK                                          
107800         END-IF                                                           
107900       END-IF                                                             
108000                                                                          
108100       MOVE MSG-SPAR-DATA (13) TO MID-KDMFSFOR                            
108200     ELSE                                                                 
108300       MOVE 'W' TO MSG-SPAR-KDTRANS                                       
108400       MOVE MSG-AREA TO MSG-SPAR-1                                        
108500       MOVE SPACE TO MFS-KDTRTYP                                          
108600                     MFS-IDPFK                                            
108700       MOVE +2 TO INDX                                                    
108800                                                                          
108900       PERFORM UNTIL MSG-SPAR-DATA (INDX) = SPACE                         
109000         MOVE MSG-SPAR-DATA (INDX) TO MSG-SPAR-KDTRANS-TAB (INDX)         
109100         ADD +1 TO INDX                                                   
109200       END-PERFORM                                                        
109300                                                                          
109400       PERFORM UNTIL MSG-SPAR-DATA (INDX) NOT = SPACE                     
109500         ADD +1 TO INDX                                                   
109600       END-PERFORM                                                        
109700                                                                          
109800       IF INDX > 9                                                        
109900         MOVE +9 TO INDX                                                  
110000       END-IF                                                             
110100                                                                          
110200       MOVE MSG-SPAR-DATA (INDX) TO MID-IDTRANS-POS-1                     
110300       ADD +1 TO INDX                                                     
110400       MOVE MSG-SPAR-DATA (INDX) TO MID-IDTRANS-POS-2                     
110500       ADD +1 TO INDX                                                     
110600       MOVE MSG-SPAR-DATA (INDX) TO MID-IDTRANS-POS-3                     
110700       ADD +1 TO INDX                                                     
110800       MOVE MSG-SPAR-DATA (INDX) TO MID-IDTRANS-POS-4                     
110900       ADD +1 TO INDX                                                     
111000       MOVE MSG-SPAR-DATA (INDX) TO MID-KDMFSFOR                          
111100       IF MID-IDTRANS-POS-1 = ' '                                         
111200         MOVE 'X' TO MFS-IDPFK                                            
111300       END-IF                                                             
111400     END-IF                                                               
111500                                                                          
111600     IF MID-KDMFSFOR NOT = '2'                                            
111700       IF (W-IDUSER-POS-1-2 = 'PH' OR 'MW')                               
111800           AND MID-KDMFSFOR NOT = '1'                                     
111900         MOVE '2' TO MID-KDMFSFOR                                         
112000       ELSE                                                               
112100         MOVE '1' TO MID-KDMFSFOR                                         
112200       END-IF                                                             
112300     END-IF                                                               
112400                                                                          
112500     IF MID-IDTRANS-POS-2 = SPACE                                         
112600       MOVE '0' TO MID-IDTRANS-POS-2                                      
112700     END-IF                                                               
112800                                                                          
112900     IF MID-IDTRANS-POS-3 = SPACE                                         
113000       MOVE '0' TO MID-IDTRANS-POS-3                                      
113100     END-IF                                                               
113200                                                                          
113300     IF MID-IDTRANS-POS-4 = SPACE                                         
113400       MOVE '0' TO MID-IDTRANS-POS-4                                      
113500     END-IF                                                               
113600                                                                          
113700     MOVE MID-IDTRANS TO MFS-IDTRANS                                      
113800     MOVE MID-KDMFSFOR TO MFS-KDMFSFOR                                    
113900     .                                                                    
114000     EJECT                                                                
114100 B-SOEK-TRANS-I-TABELL SECTION.                                           
114200                                                                          
114300     SET TAB-IX TO 1                                                      
114400     SEARCH ALL MODNAMN-TAB                                               
114500       AT END                                                             
114600         MOVE 'X' TO MFS-IDPFK                                            
114700       WHEN TAB-IDTRANS (TAB-IX) = MID-IDTRANS                            
114800         MOVE TAB-MODNAMN (TAB-IX) TO MFS-IDMOD                           
114900     END-SEARCH                                                           
115000                                                                          
115100     IF ENGLISH-TEXT    AND TAB-HOMR (TAB-IX) = 'N'                       
115200       MOVE TAB-HOMR (TAB-IX) TO MFS-KDHUVOMR                             
115300       IF MID-IDTRANS = '0501'                                            
115400         MOVE 'WELOGON ' TO MFS-IDMOD                                     
115500       ELSE                                                               
115600         IF MID-IDTRANS = '0502'                                          
115700           MOVE 'WEPW    ' TO MFS-IDMOD                                   
115800         ELSE                                                             
115900           IF MID-IDTRANS = '0504'                                        
116000             MOVE 'WMENU   ' TO MFS-IDMOD                                 
116100           END-IF                                                         
116200         END-IF                                                           
116300       END-IF                                                             
116400     END-IF                                                               
116500                                                                          
116600     MOVE MID-IDTRANS TO MOD-IDTRANS                                      
116700     IF TAB-HOMR (TAB-IX) = 'S'                                           
116800       MOVE MFS-KDMFSFOR TO MOD-IDLANDX2                                  
116900     ELSE                                                                 
117000       MOVE MFS-RENSA-FAELT TO MOD-IDLANDX2                               
117100     END-IF                                                               
117200     MOVE +48 TO MSG-KVLL                                                 
117300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
117400     .                                                                    
117500     EJECT                                                                
117600 D-PROG-TO-PROG SECTION.                                                  
117700     MOVE NEJ TO P-TO-P-SW                                                
117800     IF MID-IDTRANS > '9900'                                              
117900       MOVE SPACE TO MID-KDMFSFOR                                         
118000     END-IF                                                               
118100     EVALUATE MID-MODNAMN-NR                                              
118200       WHEN '05031' MOVE W-P-TO-P-05031 TO MSG-IO-AREA                    
118300                    PERFORM S-INSERT-ALTMSG                               
118400       WHEN '05032' MOVE W-P-TO-P-05032 TO MSG-IO-AREA                    
118500                    PERFORM S-INSERT-ALTMSG                               
118600       WHEN '08131' PERFORM DC-FIXA-BILD-ATTR                             
118700                    MOVE 'W0O81301' TO MFS-IDMOD                          
118800                    PERFORM IMS-INSERT-MSG                                
118900       WHEN '08132' PERFORM DC-FIXA-BILD-ATTR                             
119000                    MOVE 'W0O813N1' TO MFS-IDMOD                          
119100                    PERFORM IMS-INSERT-MSG                                
119200       WHEN '42131' PERFORM DC-FIXA-BILD-ATTR                             
119300                    MOVE 'W4O21301' TO MFS-IDMOD                          
119400                    PERFORM IMS-INSERT-MSG                                
119500       WHEN '42132' PERFORM DC-FIXA-BILD-ATTR                             
119600                    MOVE 'W4O213N1' TO MFS-IDMOD                          
119700                    PERFORM IMS-INSERT-MSG                                
119800       WHEN '42231' PERFORM DC-FIXA-BILD-ATTR                             
119900                    MOVE 'W4O22301' TO MFS-IDMOD                          
120000                    PERFORM IMS-INSERT-MSG                                
120100       WHEN '42232' PERFORM DC-FIXA-BILD-ATTR                             
120200                    MOVE 'W4O223N1' TO MFS-IDMOD                          
120300                    PERFORM IMS-INSERT-MSG                                
120400       WHEN '42331' PERFORM DC-FIXA-BILD-ATTR                             
120500                    MOVE 'W4O23301' TO MFS-IDMOD                          
120600                    PERFORM IMS-INSERT-MSG                                
120700       WHEN '42332' PERFORM DC-FIXA-BILD-ATTR                             
120800                    MOVE 'W4O233N1' TO MFS-IDMOD                          
120900                    PERFORM IMS-INSERT-MSG                                
121000       WHEN '42431' PERFORM DC-FIXA-BILD-ATTR                             
121100                    MOVE 'W4O24301' TO MFS-IDMOD                          
121200                    PERFORM IMS-INSERT-MSG                                
121300       WHEN '42432' PERFORM DC-FIXA-BILD-ATTR                             
121400                    MOVE 'W4O243N1' TO MFS-IDMOD                          
121500                    PERFORM IMS-INSERT-MSG                                
121600       WHEN '45331' PERFORM DC-FIXA-BILD-ATTR                             
121700                    MOVE 'W4O53301' TO MFS-IDMOD                          
121800                    PERFORM IMS-INSERT-MSG                                
121900       WHEN '45332' PERFORM DC-FIXA-BILD-ATTR                             
122000                    MOVE 'W4O533N1' TO MFS-IDMOD                          
122100                    PERFORM IMS-INSERT-MSG                                
122200       WHEN '45351' PERFORM DC-FIXA-BILD-ATTR                             
122300                    MOVE 'W4O53501' TO MFS-IDMOD                          
122400                    PERFORM IMS-INSERT-MSG                                
122500       WHEN '45352' PERFORM DC-FIXA-BILD-ATTR                             
122600                    MOVE 'W4O535N1' TO MFS-IDMOD                          
122700                    PERFORM IMS-INSERT-MSG                                
122800       WHEN '98711'                                                       
122900                  MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-98711           
123000                  IF MID-98711-KOMMA = ','                                
123100                    MOVE MID-98711-KEY TO W-98711-KEY                     
123200                    IF MID-98711-IDTRANS = SPACE                          
123300                      MOVE '21021' TO MID-98711-IDTRANS                   
123400                    END-IF                                                
123500                  ELSE                                                    
123600                    MOVE MID-98711 TO W-98711-KEY                         
123700                    MOVE '2102' TO MID-98711-IDTRANS                      
123800                    MOVE '1' TO MID-98711-POS-5                           
123900                  END-IF                                                  
124000                  STRING 'W' MID-98711-POS-1                              
124100                         'T' MID-98711-POS-2-4                            
124200                         '  9871' MID-98711-POS-5                         
124300                         DELIMITED BY SIZE INTO W-98711-TRAN              
124400                  END-STRING                                              
124500                  PERFORM UNTIL W-98711-9 NUMERIC                         
124600                    MOVE W-98711-8 TO W-98711-9                           
124700                    MOVE W-98711-7 TO W-98711-8                           
124800                    MOVE W-98711-6 TO W-98711-7                           
124900                    MOVE W-98711-5 TO W-98711-6                           
125000                    MOVE W-98711-4 TO W-98711-5                           
125100                    MOVE W-98711-3 TO W-98711-4                           
125200                    MOVE W-98711-2 TO W-98711-3                           
125300                    MOVE W-98711-1 TO W-98711-2                           
125400                    MOVE ZERO      TO W-98711-1                           
125500                  END-PERFORM                                             
125600                  INSPECT W-98711-KEY REPLACING ALL SPACE BY ZERO         
125700                  MOVE W-P-TO-P-98711 TO MSG-IO-AREA                      
125800                  PERFORM S-INSERT-ALTMSG                                 
125900       WHEN '9940 ' MOVE W-P-TO-P-99401 TO MSG-IO-AREA                    
126000                    PERFORM S-INSERT-ALTMSG                               
126100       WHEN '9941 ' PERFORM DB-FIXA-TILL-KEY                              
126200                    MOVE '01' TO W-99411-ID                               
126300                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
126400                    PERFORM S-INSERT-ALTMSG                               
126500       WHEN '9942 ' PERFORM DB-FIXA-TILL-KEY                              
126600                    MOVE '02' TO W-99411-ID                               
126700                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
126800                    PERFORM S-INSERT-ALTMSG                               
126900       WHEN '9943 ' PERFORM DB-FIXA-TILL-KEY                              
127000                    MOVE '03' TO W-99411-ID                               
127100                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
127200                    PERFORM S-INSERT-ALTMSG                               
127300       WHEN '9945 ' PERFORM DB-FIXA-TILL-KEY                              
127400                    MOVE '05' TO W-99411-ID                               
127500                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
127600                    PERFORM S-INSERT-ALTMSG                               
127700       WHEN '9946 ' PERFORM DB-FIXA-TILL-KEY                              
127800                    MOVE 'MCM' TO W-99411-ID                              
127900                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
128000                    PERFORM S-INSERT-ALTMSG                               
128100       WHEN '9947 ' PERFORM DB-FIXA-TILL-KEY                              
128200                    MOVE 'MCK' TO W-99411-ID                              
128300                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
128400                    PERFORM S-INSERT-ALTMSG                               
128500       WHEN '9948 ' PERFORM DB-FIXA-TILL-KEY                              
128600                    MOVE 'MMU' TO W-99411-ID                              
128700                    MOVE W-P-TO-P-99411 TO MSG-IO-AREA                    
128800                    PERFORM S-INSERT-ALTMSG                               
128900       WHEN '9970 ' MOVE W-IDUSER TO P-SW-USER-KDP                        
129000                    MOVE W-P-TO-P-99701 TO MSG-IO-AREA                    
129100                    PERFORM S-INSERT-ALTMSG                               
129200       WHEN '9971 '                                                       
129300                    MOVE W-P-TO-P-99711 TO MSG-IO-AREA                    
129400                    PERFORM S-INSERT-ALTMSG                               
129500     END-EVALUATE                                                         
129600     .                                                                    
129700     EJECT                                                                
129800 DB-FIXA-TILL-KEY SECTION.                                                
129900     MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-PVINK                      
130000     IF MID-PVINK-1 NUMERIC                                               
130100       MOVE MID-PVINK-1 TO W-KEY                                          
130200     ELSE                                                                 
130300       INSPECT MID-PVINK-2 REPLACING ALL SPACE BY ZERO                    
130400       IF MID-PVINK-2 NUMERIC                                             
130500         MOVE MID-PVINK-2 TO W-KEY                                        
130600       ELSE                                                               
130700         MOVE ZERO TO W-KEY                                               
130800       END-IF                                                             
130900     END-IF                                                               
131000     MOVE MFS-IDTRANS TO W-99411-IDTRA                                    
131100     MOVE MFS-KDMFSFOR TO W-99411-KDMFS                                   
131200     MOVE W-KEY TO W-99411-ARTNR                                          
131300     MOVE W-KEY TO W-99411-KEY1                                           
131400     .                                                                    
131500     EJECT                                                                
131600 DC-FIXA-BILD-ATTR SECTION.                                               
131700     STRING MFS-FORMATETS-ATTR MID-IDTRANS-POS-1                          
131800            MFS-FORMATETS-ATTR MID-IDTRANS-POS-2                          
131900            MFS-FORMATETS-ATTR MID-IDTRANS-POS-3                          
132000            MFS-FORMATETS-ATTR MID-IDTRANS-POS-4                          
132100            MFS-BLANKA-UT-FAELT                                           
132200              DELIMITED BY SIZE INTO MOD-FORMAT                           
132300     END-STRING                                                           
132400     MOVE JA TO P-TO-P-SW                                                 
132500     MOVE +18 TO MSG-KVLL                                                 
132600     .                                                                    
132700     EJECT                                                                
132800 G-SEND-INFO SECTION.                                                     
132900                                                                          
133000     EVALUATE MFS-IDTRANS                                                 
133100     WHEN '0602'                                                          
133200       MOVE '0602' TO MOD-IDTRANS                                         
133300       IF MFS-KDMFSFOR = '2'                                              
133400         MOVE 'Screen 0602 closed. Invoice via Bill-IT'                   
133500           TO MOD-TEMFSINF                                                
133600       ELSE                                                               
133700         MOVE 'Bild 0602 utgått. Fakt. via Bill-IT'                       
133800           TO MOD-TEMFSINF                                                
133900       END-IF                                                             
134000       MOVE +48 TO MSG-KVLL                                               
134100     WHEN OTHER                                                           
134200       IF MSG-SIGNON-USERID = 'V030174 '                                  
134300*                         OR  'PC36176 '                                  
134400         MOVE MSG-TIME-OF-DAY TO W-TIME                                   
134500*        PERFORM UNTIL W-TIME-MM < 4                                      
134600*          SUBTRACT 4 FROM W-TIME-MM                                      
134700*        END-PERFORM                                                      
134800*        IF W-TIME-MM < 1                                                 
134900*          MOVE '  JOHN ARBETAR W28-31                '                   
135000*            TO MOD-TEMFSINF                                              
135100*        ELSE                                                             
135200*          IF W-TIME-MM < 2                                               
135300*            MOVE '  ARBETAR DU?                        '                 
135400*              TO MOD-TEMFSINF                                            
135500*          ELSE                                                           
135600*            IF W-TIME-MM < 3                                             
135700*              MOVE '  VI ANDRA SEMESTRAR                 '               
135800*                TO MOD-TEMFSINF                                          
135900*            ELSE                                                         
136000*              MOVE '  HÄLSNINGAR STOREBROR              '                
136100*                TO MOD-TEMFSINF                                          
136200*            END-IF                                                       
136300*          END-IF                                                         
136400*        END-IF                                                           
136500*        MOVE +48 TO MSG-KVLL                                             
136600       END-IF                                                             
136700     END-EVALUATE                                                         
136800     .                                                                    
136900     EJECT                                                                
137000 H-SOEK-TRANS-I-MENY SECTION.                                             
137100                                                                          
137200     IF MSG-SPAR-KDTRANS1-6 = 'WHELP2' AND MFS-IDPFK = 'X'                
137300                                                                          
137400       IF MID-IDTRANS-POS-3 NOT = '0'                                     
137500         MOVE '0' TO MID-IDTRANS-POS-3                                    
137600       ELSE                                                               
137700         IF MID-IDTRANS-POS-2 NOT = '0'                                   
137800           MOVE '0' TO MID-IDTRANS-POS-2                                  
137900         ELSE                                                             
138000           MOVE ' ' TO MID-IDTRANS-POS-1                                  
138100         END-IF                                                           
138200       END-IF                                                             
138300     END-IF                                                               
138400                                                                          
138500     IF MID-IDTRANS-POS-1-3 > '990'                                       
138600       MOVE '99'  TO W-MODN-POS-7-8                                       
138700       MOVE '990' TO MID-IDTRANS                                          
138800     ELSE                                                                 
138900       MOVE +3 TO INDX                                                    
139000       PERFORM UNTIL TAB-MENY-NR (INDX) > MID-IDTRANS-POS-1-3             
139100         ADD +1 TO INDX                                                   
139200       END-PERFORM                                                        
139300       IF MFS-FIRST                                                       
139400         SUBTRACT +2 FROM INDX                                            
139500         MOVE TAB-MENY-NR (INDX) TO MID-IDTRANS-POS-1-3                   
139600       ELSE                                                               
139700         IF MFS-NEXT                                                      
139800           IF TAB-MENY-NR (INDX) NOT = '999'                              
139900             MOVE TAB-MENY-NR (INDX) TO MID-IDTRANS-POS-1-3               
140000           END-IF                                                         
140100         ELSE                                                             
140200           SUBTRACT +1 FROM INDX                                          
140300         END-IF                                                           
140400       END-IF                                                             
140500       MOVE TAB-MENYN-7-8 (INDX) TO W-MODN-POS-7-8                        
140600     END-IF                                                               
140700                                                                          
140800     MOVE MID-IDTRANS TO MOD-IDTRANS                                      
140900     MOVE MID-KDMFSFOR TO MOD-KDMFSFOR                                    
141000*    MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
141100     CALL VIMSID USING W-IMSID                                            
141200     IF W-IMSID = 'IMG0'                                                  
141300       MOVE ' Prod  PULS   Parts Universal Logistic System'               
141400*      MOVE ' Prod  PULS      WE WELCOME  JAPAN  TO PULS  '               
141500                          TO MOD-TEMFSINF                                 
141600     END-IF                                                               
141700     IF W-IMSID = 'IMB0'                                                  
141800       MOVE ' Acpt  PULS   Parts Universal Logistic System'               
141900*      MOVE ' Acpt  PULS      WE WELCOME  JAPAN  TO PULS  '               
142000                          TO MOD-TEMFSINF                                 
142100     END-IF                                                               
142200     IF W-IMSID = 'IMD0'                                                  
142300       MOVE ' Xdev  PULS   Parts Universal Logistic System'               
142400*      MOVE ' Xdev  PULS      WE WELCOME  JAPAN  TO PULS  '               
142500                          TO MOD-TEMFSINF                                 
142600     END-IF                                                               
142700     IF W-IMSID = 'IMY0'                                                  
142800       MOVE ' Igrt  PULS   Parts Universal Logistic System'               
142900*      MOVE ' Igrt  PULS      WE WELCOME  JAPAN  TO PULS  '               
143000                          TO MOD-TEMFSINF                                 
143100     END-IF                                                               
143200     IF W-IMSID = 'IMP0'                                                  
143300       MOVE ' Deve  PULS   Parts Universal Logistic System'               
143400*      MOVE ' Deve  PULS      WE WELCOME  JAPAN  TO PULS  '               
143500                          TO MOD-TEMFSINF                                 
143600     END-IF                                                               
143700     MOVE +64 TO MSG-KVLL                                                 
143800                                                                          
143900     IF ENGLISH-TEXT    AND W-MODNAMN NOT = 'W0O50401'                    
144000       MOVE 'N' TO W-MODN-POS-6                                           
144100     END-IF                                                               
144200*    IF ENGLISH-TEXT                                                      
144300*    MOVE ' SDC PROJECT IS NOW IMPLEMENTED  QUESTIONS:+4631592100'        
144400*                         TO MOD-TEMFSINF                                 
144500*    ELSE                                                                 
144600*    MOVE ' SDC PROJEKTET ÄR NU INSTALLERAT FRÅGOR: 031-592100'           
144700*                         TO MOD-TEMFSINF                                 
144800*    END-IF                                                               
144900     MOVE W-MODNAMN TO MFS-IDMOD                                          
145000                                                                          
145100     IF MSG-SPAR-KDTRANS = 'WHELP   '                                     
145200       IF W-IDUSER-POS-1 = 'D'                                            
145300       OR W-IDUSER = 'PC36176 '                                           
145400         MOVE 'W0O50493' TO MFS-IDMOD                                     
145500       END-IF                                                             
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900 S-INSERT-ALTMSG SECTION.                                                 
146000                                                                          
146100     PERFORM IMS-CHANGE-ALTMSG                                            
146200     IF STATUS-OK                                                         
146300       PERFORM IMS-INSERT-ALTMSG                                          
146400     ELSE                                                                 
146500       MOVE MSG-IO-AREA TO W-P-TO-P-SPAR                                  
146600       IF SECURITY-FEL                                                    
146700         STRING ' SECURITY VIOLATION '                                    
146800                 W-P-TO-P-S1                                              
146900                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
147000       ELSE                                                               
147100         STRING 'WRONG PICTURE '                                          
147200                 W-P-TO-P-S1                                              
147300                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
147400       END-IF                                                             
147500       MOVE MFS-IDTRANS  TO MOD-IDTRANS                                   
147600       MOVE MFS-KDMFSFOR TO MOD-KDMFSFOR                                  
147700       MOVE 'W0O50401' TO MFS-IDMOD                                       
147800       MOVE +68 TO MSG-KVLL                                               
147900       PERFORM IMS-INSERT-MSG                                             
148000     END-IF                                                               
148100     MOVE JA TO P-TO-P-SW                                                 
148200     .                                                                    
148300     EJECT                                                                
148400* IMS SEKTIONER                                                           
148500     SKIP2                                                                
148600 IMS-GET-MSG SECTION.                                                     
148700     MOVE '  QCCF' TO GODK-STATUSKODER                                    
148800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
148900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
149000     PERFORM IMS-STATUSKONTROLL                                           
149100     .                                                                    
149200     SKIP2                                                                
149300 IMS-INSERT-MSG SECTION.                                                  
149400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
149500     MOVE SPACE TO GODK-STATUSKODER                                       
149600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
149700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100 IMS-CHANGE-ALTMSG SECTION.                                               
150200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
150300     MOVE '  A1A4' TO GODK-STATUSKODER                                    
150400     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
150500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     SKIP3                                                                
150900 IMS-INSERT-ALTMSG SECTION.                                               
151000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151100     MOVE SPACE TO GODK-STATUSKODER                                       
151200     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
151300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
151400     PERFORM IMS-STATUSKONTROLL                                           
151500     .                                                                    
151600     SKIP3                                                                
151700 IMS-STATUSKONTROLL SECTION.                                              
151800                                                                          
151900     SET STATUS-IX TO 1                                                   
152000     SEARCH GODK-STATUS                                                   
152100       AT END                                                             
152200         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT                        
152300         CALL FELLOG                                                      
152400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
152500         CONTINUE                                                         
152600     END-SEARCH                                                           
152700     .                                                                    
