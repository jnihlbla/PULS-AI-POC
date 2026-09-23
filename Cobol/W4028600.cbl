000300 ID DIVISION.                                                             
000400                                                                          
000500 PROGRAM-ID.     W4028600.                                                
000600 AUTHOR.         GÖRAN KJELLSON                                           
000700 DATE-WRITTEN.   WINTER 2020                                              
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*    FUNKCION.                                                            
001100*        JUST CREATING MAIL WITH THE ANSWER FROM MIC                      
001200*        ON A DELIVERY REQUEST.                                           
001210*        THERE WILL BE THREE DIFFERENT MAILS:                             
001220*        - MIC-PART  (PART IS BLOCKED)                                    
001230*        - MIC-ORDER (ORDER IS BLOCKED)                                   
001240*        - MIC-ERROR (MIC COULD NOT PROCESS THE REQUEST                   
001250*        - MIC-FAIL  (MIC ANSWER COULD NOT BE PROCESSED                   
001300*        THE MAIL IS SENT VIA D&P.                                        
001500*                                                                         
001600*    INPUT.                                                               
001700*        TRANSACTION: W40286X                                             
001800*                                                                         
001900*    OUTPUT.                                                              
002000*        D&P: MIC-PART                                                    
002100*             MIC-ORDER                                                   
002110*             MIC-ERROR                                                   
002120*             MIC-FAIL                                                    
002210                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003100 77  IDPGM                       PIC X(08)   VALUE 'W4028600'.            
003111 01  ERROR-TEXT.                                                          
003112     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
003113     03  ERROR-TEXT-STYR         PIC X(72)   VALUE SPACE.                 
003120 77  CURRENT-SECTION             PIC X(16) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600*                                                                         
004110 01  TYPE-OF-ANSWER              PIC X(1)    VALUE SPACE.                 
004120     88 MIC-PART                             VALUE 'P'.                   
004130     88 MIC-ORDER                            VALUE 'O'.                   
004140     88 MIC-ERROR                            VALUE 'E'.                   
004141     88 MIC-FAIL                             VALUE 'F'.                   
004150                                                                          
004200 77  PART-IX                     PIC 9(3)    VALUE ZERO.                  
004500 77  PART-IX-MAX                 PIC 9(4)    VALUE 100.                   
004600 77  LINE-IX                     PIC 9(2)    VALUE ZERO.                  
004700 77  PART-LINE-MAX               PIC 9(2)    VALUE 30.                    
004701 77  PART-PART-IX                PIC 9(2)    VALUE ZERO.                  
004710 77  PART-PART-MAX               PIC 9(2)    VALUE 10.                    
004800 77  ORDER-LINE-MAX              PIC 9(2)    VALUE 30.                    
004900 77  ERROR-LINE-MAX              PIC 9(2)    VALUE 31.                    
005000 77  FAIL-LINE-MAX               PIC 9(2)    VALUE 13.                    
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  GODK-MID                            VALUE '4286'.                
010300                                                                          
011400                                                                          
011500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012110     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012120     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200                                                                          
012300*    --- PARAMETERS TO ABEND                                              
012400                                                                          
012500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012800                                                                          
014910*    --- STATUS CODES FROM IMS                                            
014920 01  STATUS-WS                   PIC XX.                                  
014930     88  SEGMENT-FOUND                       VALUE '  '.                  
014960                                                                          
014970 01  GOOD-STATUSCODES.                                                    
014980     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014990                                                                          
014991*    --- IMS FUNCTION CODES                                               
014992*01  -COPY W0003                                                          
015000                                                                          
015010 01  FILLER                      PIC X(16)   VALUE 'DAP-AREA '.           
015011 01  KDRC-DISPLAY                PIC Z(5).                                
015020 01  HDR-AREA.                                                            
015030*    03  -COPY WZ01REQU                                                   
015040*    03  -COPY WZ04HDR                                                    
015110 01  SEND-RAD                    PIC X(132)  VALUE SPACE.                 
015120                                                                          
015130 01  FILLER                      PIC X(16)   VALUE '*WZ01SEND**'.         
015140*01  -COPY WZ01SEND                                                       
015150*                            DB2 FUNKTIONSKODER                           
015200                                                                          
015210                                                                          
015300*                                                                         
015400*    --- AREOR FÖR MSG-HANTERING                                          
015500*                                                                         
015510                                                                          
015520 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
015530 01  KOM-MSG-IO-AREA.                                                     
015540*03  -COPY WMSGKOM                                                        
015600 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
015700                                                                          
015800*01  -COPY WMSGAREA                                                       
015900                                                                          
016000 01  MID-AREA.                                                            
016100     03  MID-IDTRANS PIC X(24).                                           
016700*    03  -COPY W4I28601                                                   
016800                                                                          
016900                                                                          
030900****************************************                                  
031000*  MIC PART ANSWER                     *                                  
031100****************************************                                  
031200                                                                          
031500 01  PART-MAIL-LINES.                                                     
031836     03   FILLER.                                                         
031837       05 FILLER               PIC X(50)  VALUE                           
031838          'Following part(s) have been screened and it is NOT'.           
031839       05 FILLER               PIC X(50)  VALUE                           
031840          ' ok to send the part(s) to                        '.           
031854     03   FILLER.                                                         
031855       05 FILLER               PIC X(9)   VALUE                           
031856          'district '.                                                    
031857       05 PART-IDDISTR         PIC Z(4).                                  
031858       05 FILLER               PIC X(14)  VALUE                           
031859          ' and customer '.                                               
031860       05 PART-IDKUNDNR        PIC Z(5)9.                                 
031861       05 FILLER               PIC X(8)   VALUE                           
031862          ' from DC'.                                                     
031863       05 PART-IDDC-1          PIC X(2).                                  
031864       05 FILLER               PIC X(10)  VALUE                           
031865          '.'.                                                            
031866       05 FILLER               PIC X(50)  VALUE SPACE.                    
031867     03   FILLER               PIC X(100) VALUE SPACE.                    
031868     03   FILLER               PIC X(100) VALUE SPACE.                    
031869     03   FILLER.                                                         
031870       05 FILLER               PIC X(10)  VALUE SPACE.                    
031871       05 FILLER               PIC X(40)  VALUE                           
031872          'Please follow these steps'.                                    
031873       05 FILLER               PIC X(50)  VALUE SPACE.                    
031874     03   FILLER               PIC X(100) VALUE SPACE.                    
031875     03   FILLER.                                                         
031876       05 FILLER               PIC X(10)  VALUE SPACE.                    
031877       05 FILLER               PIC X(40)  VALUE                           
031878          '1.  If the order is in status R:'.                             
031879       05 FILLER               PIC X(50)  VALUE SPACE.                    
031880     03   FILLER.                                                         
031881       05 FILLER               PIC X(15)  VALUE SPACE.                    
031882       05 FILLER               PIC X(35)  VALUE                           
031883          '1.  Please delete order line from t'.                          
031884       05 FILLER               PIC X(50)  VALUE                           
031885          'he order in PULS.                  '.                          
031886     03   FILLER.                                                         
031887       05 FILLER               PIC X(10)  VALUE SPACE.                    
031888       05 FILLER               PIC X(40)  VALUE                           
031889          '2.  If the order is in status U:'.                             
031890       05 FILLER               PIC X(50)  VALUE SPACE.                    
031891     03   FILLER.                                                         
031892       05 FILLER               PIC X(15)  VALUE SPACE.                    
031893       05 FILLER               PIC X(21)  VALUE                           
031894          '1.  Please inform DC '.                                        
031895       05 PART-IDDC-2          PIC X(2).                                  
031896       05 FILLER               PIC X(12)  VALUE                           
031897          ' to stop pro'.                                                 
031898       05 FILLER               PIC X(50)  VALUE                           
031899          'cess the order until further notice.'.                         
031900     03   FILLER.                                                         
031901       05 FILLER               PIC X(15)  VALUE SPACE.                    
031902       05 FILLER               PIC X(24)  VALUE                           
031903          '2.  It is not ok for DC '.                                     
031904       05 PART-IDDC-3          PIC X(2).                                  
031905       05 FILLER               PIC X(9)   VALUE                           
031906          ' to proce'.                                                    
031907       05 FILLER               PIC X(50)  VALUE                           
031908          'ed with the order until Global Customer support   '.           
031909     03   FILLER.                                                         
031910       05 FILLER               PIC X(15)  VALUE SPACE.                    
031911       05 FILLER               PIC X(35)  VALUE                           
031912          '    have confirmed that it is ok.  '.                          
031913       05 FILLER               PIC X(50)  VALUE SPACE.                    
031914     03   FILLER.                                                         
031915       05 FILLER               PIC X(15)  VALUE SPACE.                    
031916       05 FILLER               PIC X(35)  VALUE                           
031917          '3.  Delete affected order lines acc'.                          
031918       05 FILLER               PIC X(50)  VALUE                           
031919          'ording to the process of deleting printed lines.  '.           
031920     03   FILLER.                                                         
031921       05 FILLER               PIC X(10)  VALUE SPACE.                    
031922       05 FILLER               PIC X(40)  VALUE                           
031923          '3.  If the order is in status P or SC:'.                       
031924       05 FILLER               PIC X(50)  VALUE SPACE.                    
031925     03   FILLER.                                                         
031926       05 FILLER               PIC X(15)  VALUE SPACE.                    
031927       05 FILLER               PIC X(21)  VALUE                           
031928          '1.  Please inform DC '.                                        
031929       05 PART-IDDC-4          PIC X(2).                                  
031930       05 FILLER               PIC X(12)  VALUE                           
031931          ' to stop pro'.                                                 
031932       05 FILLER               PIC X(50)  VALUE                           
031933          'cess the order until further notice.'.                         
031934     03   FILLER.                                                         
031935       05 FILLER               PIC X(15)  VALUE SPACE.                    
031936       05 FILLER               PIC X(24)  VALUE                           
031937          '2.  It is not ok for DC '.                                     
031938       05 PART-IDDC-5          PIC X(2).                                  
031939       05 FILLER               PIC X(9)   VALUE                           
031940          ' to proce'.                                                    
031941       05 FILLER               PIC X(50)  VALUE                           
031942          'ed with the order until Global Customer support   '.           
031943     03   FILLER.                                                         
031944       05 FILLER               PIC X(15)  VALUE SPACE.                    
031945       05 FILLER               PIC X(35)  VALUE                           
031946          '    have confirmed that it is ok.  '.                          
031947       05 FILLER               PIC X(50)  VALUE SPACE.                    
031948     03   FILLER.                                                         
031949       05 FILLER               PIC X(15)  VALUE SPACE.                    
031950       05 FILLER               PIC X(32)  VALUE                           
031951          '3.  Move part to a new case no. '.                             
031952       05 FILLER               PIC X(53)  VALUE                           
031953          'notify Business application manager about the '.               
031954     03   FILLER.                                                         
031955       05 FILLER               PIC X(15)  VALUE SPACE.                    
031956       05 FILLER               PIC X(35)  VALUE                           
031957          '    case for them to take further  '.                          
031958       05 FILLER               PIC X(50)  VALUE                           
031959          ' action.                                          '.           
031960     03   FILLER.                                                         
031961       05 FILLER               PIC X(10)  VALUE SPACE.                    
031962       05 FILLER               PIC X(40)  VALUE                           
031963          '4.  If the order is in status S or SF:'.                       
031964       05 FILLER               PIC X(50)  VALUE SPACE.                    
031965     03   FILLER.                                                         
031966       05 FILLER               PIC X(15)  VALUE SPACE.                    
031967       05 FILLER               PIC X(21)  VALUE                           
031968          '1.  Please inform DC '.                                        
031969       05 PART-IDDC-6          PIC X(2).                                  
031970       05 FILLER               PIC X(12)  VALUE                           
031971          ' to stop pro'.                                                 
031972       05 FILLER               PIC X(50)  VALUE                           
031973          'cess the order until further notice.'.                         
031974     03   FILLER.                                                         
031975       05 FILLER               PIC X(15)  VALUE SPACE.                    
031976       05 FILLER               PIC X(24)  VALUE                           
031977          '2.  It is not ok for DC '.                                     
031978       05 PART-IDDC-7          PIC X(2).                                  
031979       05 FILLER               PIC X(9)   VALUE                           
031980          ' to proce'.                                                    
031981       05 FILLER               PIC X(50)  VALUE                           
031982          'ed with the order until Global Customer Support   '.           
031983     03   FILLER.                                                         
031984       05 FILLER               PIC X(15)  VALUE SPACE.                    
031985       05 FILLER               PIC X(35)  VALUE                           
031986          '    have confirmed that it is ok.  '.                          
031987       05 FILLER               PIC X(50)  VALUE SPACE.                    
031988     03   FILLER.                                                         
031989       05 FILLER               PIC X(15)  VALUE SPACE.                    
031990       05 FILLER               PIC X(35)  VALUE                           
031991          '3.  Please remove physical part fro'.                          
031992       05 FILLER               PIC X(50)  VALUE                           
031993          'm shipment and inform transport department'.                   
031994     03   FILLER.                                                         
031995       05 FILLER               PIC X(15)  VALUE SPACE.                    
031996       05 FILLER               PIC X(35)  VALUE                           
031997          '    manager.'                       .                          
031998       05 FILLER               PIC X(50)  VALUE SPACE.                    
031999     03   FILLER               PIC X(100) VALUE SPACE.                    
032000     03   FILLER               PIC X(100) VALUE SPACE.                    
032001     03   FILLER               PIC X(100) VALUE                           
032002          'Please always inform the customer about our actions. '.        
032003     03   FILLER               PIC X(100) VALUE SPACE.                    
032004     03   FILLER.                                                         
032005       05 FILLER               PIC X(50)  VALUE                           
032006          'Also inform the procurer to block tha part for the'.           
032007       05 FILLER               PIC X(50)  VALUE                           
032008          ' district and customer.            '.                          
032009                                                                          
032010                                                                          
032011                                                                          
032012 01  PART-MAIL-TAB REDEFINES PART-MAIL-LINES.                             
032013     03  PART-LINE OCCURS 30 TIMES.                                       
032020         05 FILLER             PIC X(100).                                
032100                                                                          
032200                                                                          
032457                                                                          
032458 01  PART-PART-LINE.                                                      
032459     03  PART-PART OCCURS 10 TIMES.                                       
032460       05 PART-IDARTNR         PIC Z(8).                                  
032461       05 FILLER               PIC X(2)   VALUE SPACE.                    
032463                                                                          
032464                                                                          
032465                                                                          
032470****************************************                                  
032500*  MIC ORDER ANSWER                    *                                  
032600****************************************                                  
032700                                                                          
032800 01  ORDER-MAIL-LINES.                                                    
032994     03   FILLER.                                                         
033030       05 FILLER               PIC X(9)   VALUE                           
033040          'District '.                                                    
033050       05 ORDER-IDDISTR        PIC Z(4).                                  
033060       05 FILLER               PIC X(14)  VALUE                           
033070          ' and Customer '.                                               
033080       05 ORDER-IDKUNDNR       PIC Z(5)9.                                 
033090       05 FILLER               PIC X(17)  VALUE                           
033091          ' have been screen'.                                            
033092       05 FILLER               PIC X(50)  VALUE                           
033093          'ed and it is NOT ok to ship'.                                  
033097     03   FILLER.                                                         
033098       05 FILLER               PIC X(33)  VALUE                           
033099          'any parts to the customer from DC'.                            
033100       05 ORDER-IDDC-1         PIC X(2).                                  
033101       05 FILLER               PIC X(15)  VALUE                           
033102          '.'.                                                            
033104     03   FILLER               PIC X(50)  VALUE SPACE.                    
033105     03   FILLER               PIC X(100) VALUE SPACE.                    
033106     03   FILLER               PIC X(100) VALUE SPACE.                    
033107     03   FILLER.                                                         
033108       05 FILLER               PIC X(10)  VALUE SPACE.                    
033109       05 FILLER               PIC X(40)  VALUE                           
033110          'Please follow these steps'.                                    
033111       05 FILLER               PIC X(50)  VALUE SPACE.                    
033112     03   FILLER               PIC X(100) VALUE SPACE.                    
033113     03   FILLER.                                                         
033120       05 FILLER               PIC X(10)  VALUE SPACE.                    
033130       05 FILLER               PIC X(40)  VALUE                           
033140          '1.  If the order is in status R:'.                             
033150       05 FILLER               PIC X(50)  VALUE SPACE.                    
033160     03   FILLER.                                                         
033170       05 FILLER               PIC X(15)  VALUE SPACE.                    
033180       05 FILLER               PIC X(35)  VALUE                           
033190          '1.  Please delete the order in PULS'.                          
033191       05 FILLER               PIC X(50)  VALUE                           
033192          '.                                  '.                          
033193     03   FILLER.                                                         
033194       05 FILLER               PIC X(10)  VALUE SPACE.                    
033195       05 FILLER               PIC X(40)  VALUE                           
033196          '2.  If the order is in status U:'.                             
033197       05 FILLER               PIC X(50)  VALUE SPACE.                    
033199     03   FILLER.                                                         
033200       05 FILLER               PIC X(15)  VALUE SPACE.                    
033210       05 FILLER               PIC X(21)  VALUE                           
033220          '1.  Please inform DC '.                                        
033230       05 ORDER-IDDC-2         PIC X(2).                                  
033240       05 FILLER               PIC X(12)  VALUE                           
033250          ' to stop pro'.                                                 
033260       05 FILLER               PIC X(50)  VALUE                           
033270          'cess the order until further notice.'.                         
033280     03   FILLER.                                                         
033290       05 FILLER               PIC X(15)  VALUE SPACE.                    
033291       05 FILLER               PIC X(24)  VALUE                           
033292          '2.  It is NOT ok for DC '.                                     
033293       05 ORDER-IDDC-3         PIC X(2).                                  
033294       05 FILLER               PIC X(9)   VALUE                           
033295          ' to proce'.                                                    
033296       05 FILLER               PIC X(50)  VALUE                           
033297          'ed with the order until Global Customer Support   '.           
033299     03   FILLER.                                                         
033300       05 FILLER               PIC X(15)  VALUE SPACE.                    
033301       05 FILLER               PIC X(35)  VALUE                           
033310          '    have confirmed that it is ok.  '.                          
033320       05 FILLER               PIC X(50)  VALUE SPACE.                    
033330     03   FILLER.                                                         
033340       05 FILLER               PIC X(15)  VALUE SPACE.                    
033350       05 FILLER               PIC X(35)  VALUE                           
033360          '3.  Delete affected order lines acc'.                          
033370       05 FILLER               PIC X(50)  VALUE                           
033380          'ording to the process of deleting printed lines.  '.           
033390     03   FILLER.                                                         
033392       05 FILLER               PIC X(10)  VALUE SPACE.                    
033393       05 FILLER               PIC X(40)  VALUE                           
033394          '3.  If the order is in status P or SC:'.                       
033395       05 FILLER               PIC X(50)  VALUE SPACE.                    
033396     03   FILLER.                                                         
033397       05 FILLER               PIC X(15)  VALUE SPACE.                    
033399       05 FILLER               PIC X(21)  VALUE                           
033400          '1.  Please inform DC '.                                        
033401       05 ORDER-IDDC-4         PIC X(2).                                  
033410       05 FILLER               PIC X(12)  VALUE                           
033420          ' to stop pro'.                                                 
033430       05 FILLER               PIC X(50)  VALUE                           
033440          'cess the order until further notice.'.                         
033450     03   FILLER.                                                         
033460       05 FILLER               PIC X(15)  VALUE SPACE.                    
033470       05 FILLER               PIC X(24)  VALUE                           
033480          '2.  It is NOT ok for DC '.                                     
033490       05 ORDER-IDDC-5         PIC X(2).                                  
033491       05 FILLER               PIC X(9)   VALUE                           
033492          ' to proce'.                                                    
033493       05 FILLER               PIC X(50)  VALUE                           
033494          'ed with the order until Global Customer Support   '.           
033496     03   FILLER.                                                         
033497       05 FILLER               PIC X(15)  VALUE SPACE.                    
033498       05 FILLER               PIC X(35)  VALUE                           
033499          '    have confirmed that it is ok.  '.                          
033500       05 FILLER               PIC X(50)  VALUE SPACE.                    
033501     03   FILLER.                                                         
033510       05 FILLER               PIC X(15)  VALUE SPACE.                    
033520       05 FILLER               PIC X(35)  VALUE                           
033530          '3.  Notify Business application man'.                          
033540       05 FILLER               PIC X(50)  VALUE                           
033550          'ager about the cases for them to take further '.               
033560     03   FILLER.                                                         
033570       05 FILLER               PIC X(15)  VALUE SPACE.                    
033580       05 FILLER               PIC X(35)  VALUE                           
033590          '    action.'.                                                  
033591       05 FILLER               PIC X(50)  VALUE SPACE.                    
033593     03   FILLER.                                                         
033594       05 FILLER               PIC X(10)  VALUE SPACE.                    
033595       05 FILLER               PIC X(40)  VALUE                           
033596          '4.  If the order is in status S or SF:'.                       
033597       05 FILLER               PIC X(50)  VALUE SPACE.                    
033599     03   FILLER.                                                         
033600       05 FILLER               PIC X(15)  VALUE SPACE.                    
033610       05 FILLER               PIC X(21)  VALUE                           
033620          '1.  Please inform DC '.                                        
033630       05 ORDER-IDDC-6         PIC X(2).                                  
033640       05 FILLER               PIC X(12)  VALUE                           
033650          ' to stop pro'.                                                 
033660       05 FILLER               PIC X(50)  VALUE                           
033670          'cess the order until further notice.'.                         
033680     03   FILLER.                                                         
033690       05 FILLER               PIC X(15)  VALUE SPACE.                    
033691       05 FILLER               PIC X(24)  VALUE                           
033692          '2.  It is not ok for DC '.                                     
033693       05 ORDER-IDDC-7         PIC X(2).                                  
033694       05 FILLER               PIC X(9)   VALUE                           
033695          ' to proce'.                                                    
033696       05 FILLER               PIC X(50)  VALUE                           
033697          'ed with the order until Global Customer Support   '.           
033699     03   FILLER.                                                         
033700       05 FILLER               PIC X(15)  VALUE SPACE.                    
033701       05 FILLER               PIC X(35)  VALUE                           
033710          '    have confirmed that it is ok.  '.                          
033720       05 FILLER               PIC X(50)  VALUE SPACE.                    
033730     03   FILLER.                                                         
033740       05 FILLER               PIC X(15)  VALUE SPACE.                    
033750       05 FILLER               PIC X(35)  VALUE                           
033760          '3.  Please remove physical parts fr'.                          
033770       05 FILLER               PIC X(50)  VALUE                           
033780          'om shipment and inform transport department    '.              
033790     03   FILLER.                                                         
033791       05 FILLER               PIC X(15)  VALUE SPACE.                    
033792       05 FILLER               PIC X(35)  VALUE                           
033793          '    manager.                       '.                          
033794       05 FILLER               PIC X(50)  VALUE SPACE.                    
033795     03   FILLER               PIC X(100) VALUE SPACE.                    
033796     03   FILLER               PIC X(100) VALUE SPACE.                    
033797     03   FILLER               PIC X(100) VALUE                           
033798          'Please always inform the customer about our actions. '.        
033799     03   FILLER               PIC X(100) VALUE SPACE.                    
033800     03   FILLER.                                                         
033810       05 FILLER               PIC X(50)  VALUE                           
033820          'Also inform the procurer to block all parts for th'.           
033830       05 FILLER               PIC X(50)  VALUE                           
033840          'e district and customer until further notice.'.                
033850                                                                          
033860                                                                          
033880                                                                          
033890 01  ORDER-MAIL-TAB REDEFINES ORDER-MAIL-LINES.                           
033891     03  ORDER-LINE OCCURS 30 TIMES.                                      
033892         05 FILLER             PIC X(100).                                
033893                                                                          
033894                                                                          
033895                                                                          
033896****************************************                                  
033897*  MIC ERROR ANSWER                    *                                  
033898****************************************                                  
033899                                                                          
033900 01  ERROR-MAIL-LINES.                                                    
033901     03   FILLER.                                                         
033902       05 FILLER               PIC X(9)   VALUE                           
033903          'District '.                                                    
033904       05 ERROR-IDDISTR        PIC Z(4).                                  
033905       05 FILLER               PIC X(11)  VALUE                           
033906          ', Customer '.                                                  
033907       05 ERROR-IDKUNDNR       PIC Z(6)9.                                 
033908       05 FILLER               PIC X(8)   VALUE                           
033909          ', order '.                                                     
033910       05 ERROR-IDORDNR7       PIC Z(5)9.                                 
033912       05 FILLER               PIC X(7)   VALUE                           
033913          ' have f'.                                                      
033915       05 FILLER               PIC X(50)  VALUE                           
033916          'ailed to be screened. Please contact MIC to see if'.           
033918     03   FILLER.                                                         
033919       05 FILLER               PIC X(50)  VALUE                           
033920          ' it is okay to send the order to customer. If not '.           
033922       05 FILLER               PIC X(50)  VALUE                           
033923          'follow steps below                                '.           
033924     03   FILLER               PIC X(100) VALUE SPACE.                    
033925     03   FILLER               PIC X(100) VALUE SPACE.                    
033927     03   FILLER.                                                         
033928       05 FILLER               PIC X(10)  VALUE SPACE.                    
033929       05 FILLER               PIC X(40)  VALUE                           
033930          'Please follow these steps'.                                    
033931       05 FILLER               PIC X(50)  VALUE SPACE.                    
033932     03   FILLER               PIC X(100) VALUE SPACE.                    
033933     03   FILLER.                                                         
033934       05 FILLER               PIC X(10)  VALUE SPACE.                    
033935       05 FILLER               PIC X(40)  VALUE                           
033936          '1.  If the order is in status R:'.                             
033937       05 FILLER               PIC X(50)  VALUE SPACE.                    
033938     03   FILLER.                                                         
033939       05 FILLER               PIC X(15)  VALUE SPACE.                    
033940       05 FILLER               PIC X(35)  VALUE                           
033941          '1.  Please delete the order in PULS'.                          
033942       05 FILLER               PIC X(50)  VALUE                           
033943          '.                                  '.                          
033945     03   FILLER.                                                         
033946       05 FILLER               PIC X(10)  VALUE SPACE.                    
033947       05 FILLER               PIC X(40)  VALUE                           
033948          '2.  If the order is in status U:'.                             
033949       05 FILLER               PIC X(50)  VALUE SPACE.                    
033950     03   FILLER.                                                         
033951       05 FILLER               PIC X(15)  VALUE SPACE.                    
033952       05 FILLER               PIC X(21)  VALUE                           
033953          '1.  Please inform DC '.                                        
033954       05 ERROR-IDDC-1         PIC X(2).                                  
033955       05 FILLER               PIC X(12)  VALUE                           
033956          ' to stop pro'.                                                 
033957       05 FILLER               PIC X(50)  VALUE                           
033958          'cess the order until further notice.'.                         
033959     03   FILLER.                                                         
033960       05 FILLER               PIC X(15)  VALUE SPACE.                    
033961       05 FILLER               PIC X(24)  VALUE                           
033962          '2.  It is NOT ok for DC '.                                     
033963       05 ERROR-IDDC-2         PIC X(2).                                  
033964       05 FILLER               PIC X(9)   VALUE                           
033965          ' to proce'.                                                    
033966       05 FILLER               PIC X(50)  VALUE                           
033967          'ed with the order until Global Customer Support   '.           
033969     03   FILLER.                                                         
033970       05 FILLER               PIC X(15)  VALUE SPACE.                    
033971       05 FILLER               PIC X(35)  VALUE                           
033972          '    have confirmed that it is ok.  '.                          
033973       05 FILLER               PIC X(50)  VALUE SPACE.                    
033974     03   FILLER.                                                         
033975       05 FILLER               PIC X(15)  VALUE SPACE.                    
033976       05 FILLER               PIC X(35)  VALUE                           
033977          '3.  Delete affected order lines acc'.                          
033978       05 FILLER               PIC X(50)  VALUE                           
033979          'ording to the process of deleting printed lines.  '.           
033980     03   FILLER.                                                         
033981       05 FILLER               PIC X(10)  VALUE SPACE.                    
033982       05 FILLER               PIC X(40)  VALUE                           
033983          '3.  If the order is in status P or SC:'.                       
033984       05 FILLER               PIC X(50)  VALUE SPACE.                    
033986     03   FILLER.                                                         
033987       05 FILLER               PIC X(15)  VALUE SPACE.                    
033988       05 FILLER               PIC X(21)  VALUE                           
033989          '1.  Please inform DC '.                                        
033990       05 ERROR-IDDC-3         PIC X(2).                                  
033991       05 FILLER               PIC X(12)  VALUE                           
033992          ' to stop pro'.                                                 
033993       05 FILLER               PIC X(50)  VALUE                           
033994          'cess the order until further notice.'.                         
033995     03   FILLER.                                                         
033996       05 FILLER               PIC X(15)  VALUE SPACE.                    
033997       05 FILLER               PIC X(24)  VALUE                           
033998          '2.  It is NOT ok for DC '.                                     
033999       05 ERROR-IDDC-4         PIC X(2).                                  
034000       05 FILLER               PIC X(9)   VALUE                           
034001          ' to proce'.                                                    
034002       05 FILLER               PIC X(50)  VALUE                           
034003          'ed with the order until Global Customer Support   '.           
034004     03   FILLER.                                                         
034005       05 FILLER               PIC X(15)  VALUE SPACE.                    
034006       05 FILLER               PIC X(35)  VALUE                           
034007          '    have confirmed that it is ok.  '.                          
034008       05 FILLER               PIC X(50)  VALUE SPACE.                    
034010     03   FILLER.                                                         
034011       05 FILLER               PIC X(15)  VALUE SPACE.                    
034012       05 FILLER               PIC X(35)  VALUE                           
034013          '3.  Notify Business application man'.                          
034014       05 FILLER               PIC X(50)  VALUE                           
034015          'ager about the cases for them to take further '.               
034016     03   FILLER.                                                         
034017       05 FILLER               PIC X(15)  VALUE SPACE.                    
034018       05 FILLER               PIC X(35)  VALUE                           
034019          '    action.'.                                                  
034020       05 FILLER               PIC X(50)  VALUE SPACE.                    
034021     03   FILLER.                                                         
034022       05 FILLER               PIC X(10)  VALUE SPACE.                    
034023       05 FILLER               PIC X(40)  VALUE                           
034024          '4.  If the order is in status S or SF:'.                       
034025       05 FILLER               PIC X(50)  VALUE SPACE.                    
034027     03   FILLER.                                                         
034028       05 FILLER               PIC X(15)  VALUE SPACE.                    
034029       05 FILLER               PIC X(21)  VALUE                           
034030          '1.  Please inform DC '.                                        
034031       05 ERROR-IDDC-5         PIC X(2).                                  
034032       05 FILLER               PIC X(12)  VALUE                           
034033          ' to stop pro'.                                                 
034034       05 FILLER               PIC X(50)  VALUE                           
034035          'cess the order until further notice.'.                         
034036     03   FILLER.                                                         
034037       05 FILLER               PIC X(15)  VALUE SPACE.                    
034038       05 FILLER               PIC X(24)  VALUE                           
034039          '2.  It is NOT ok for DC '.                                     
034040       05 ERROR-IDDC-6         PIC X(2).                                  
034041       05 FILLER               PIC X(9)   VALUE                           
034042          ' to proce'.                                                    
034043       05 FILLER               PIC X(50)  VALUE                           
034044          'ed with the order until Global Customer Support   '.           
034046     03   FILLER.                                                         
034047       05 FILLER               PIC X(15)  VALUE SPACE.                    
034048       05 FILLER               PIC X(35)  VALUE                           
034049          '    have confirmed that it is ok.  '.                          
034050       05 FILLER               PIC X(50)  VALUE SPACE.                    
034051     03   FILLER.                                                         
034052       05 FILLER               PIC X(15)  VALUE SPACE.                    
034053       05 FILLER               PIC X(35)  VALUE                           
034054          '3.  Please remove physical parts fr'.                          
034055       05 FILLER               PIC X(50)  VALUE                           
034056          'om shipment and inform transport department    '.              
034057     03   FILLER.                                                         
034058       05 FILLER               PIC X(15)  VALUE SPACE.                    
034059       05 FILLER               PIC X(35)  VALUE                           
034060          '    manager.                       '.                          
034061       05 FILLER               PIC X(50)  VALUE SPACE.                    
034062     03   FILLER               PIC X(100) VALUE SPACE.                    
034063     03   FILLER               PIC X(100) VALUE SPACE.                    
034064     03   FILLER               PIC X(100) VALUE                           
034065          'Please always inform customer about our actions. '.            
034066     03   FILLER               PIC X(100) VALUE SPACE.                    
034067     03   FILLER.                                                         
034068       05 FILLER               PIC X(50)  VALUE                           
034069          'Also inform the procurer to block all parts for th'.           
034070       05 FILLER               PIC X(50)  VALUE                           
034071          'e district and and customer until further notice. '.           
034076                                                                          
034077 01  ERROR-MAIL-TAB REDEFINES ERROR-MAIL-LINES.                           
034078     03  ERROR-LINE OCCURS 31 TIMES.                                      
034079         05 FILLER             PIC X(100).                                
034080                                                                          
034100                                                                          
034200                                                                          
034300                                                                          
034400                                                                          
034500****************************************                                  
034600*  MIC FAIL ANSWER                     *                                  
034700****************************************                                  
034800                                                                          
034900 01  FAIL-MAIL-LINES.                                                     
035000     03   FILLER.                                                         
035100       05 FILLER               PIC X(50)  VALUE                           
035200          'Received screening is not possible to process:    '.           
035300       05 FILLER               PIC X(50)  VALUE SPACE.                    
036900     03   FILLER               PIC X(100) VALUE SPACE.                    
037000     03   FILLER               PIC X(100) VALUE SPACE.                    
037100     03   FILLER.                                                         
037200       05 FILLER               PIC X(10)  VALUE SPACE.                    
037300       05 FILLER               PIC X(13)  VALUE                           
037400          'District:    '.                                                
037500       05 FAIL-IDDISTR         PIC 9(4)   VALUE ZERO.                     
037510       05 FILLER               PIC X(73)  VALUE SPACE.                    
037600     03   FILLER.                                                         
037700       05 FILLER               PIC X(10)  VALUE SPACE.                    
037800       05 FILLER               PIC X(13)  VALUE                           
037900          'Customer:    '.                                                
038000       05 FAIL-IDKUNDNR        PIC 9(6)   VALUE ZERO.                     
038010       05 FILLER               PIC X(71)  VALUE SPACE.                    
038100     03   FILLER.                                                         
038200       05 FILLER               PIC X(10)  VALUE SPACE.                    
038300       05 FILLER               PIC X(13)  VALUE                           
038400          'Order no:    '.                                                
038500       05 FAIL-IDORDNR7        PIC 9(7)   VALUE ZERO.                     
038510       05 FILLER               PIC X(70)  VALUE SPACE.                    
038600     03   FILLER.                                                         
038700       05 FILLER               PIC X(10)  VALUE SPACE.                    
038800       05 FILLER               PIC X(13)  VALUE                           
038900          'WH:          '.                                                
039000       05 FAIL-IDDC            PIC X(2)   VALUE SPACE.                    
039010       05 FILLER               PIC X(75)  VALUE SPACE.                    
039100     03   FILLER.                                                         
039200       05 FILLER               PIC X(10)  VALUE SPACE.                    
039300       05 FILLER               PIC X(13)  VALUE                           
039400          'Timestamp:   '.                                                
039500       05 FAIL-TIMESTAMP       PIC X(19)  VALUE SPACE.                    
039510       05 FILLER               PIC X(58)  VALUE SPACE.                    
039600     03   FILLER.                                                         
039700       05 FILLER               PIC X(10)  VALUE SPACE.                    
039800       05 FILLER               PIC X(13)  VALUE                           
039900          'Response:    '.                                                
040000       05 FAIL-FLRESP          PIC X(1)   VALUE SPACE.                    
040010       05 FILLER               PIC X(76)  VALUE SPACE.                    
040100     03   FILLER.                                                         
040200       05 FILLER               PIC X(10)  VALUE SPACE.                    
040300       05 FILLER               PIC X(13)  VALUE                           
040400          'Status:      '.                                                
040500       05 FAIL-FLSTATUS        PIC X(1)   VALUE SPACE.                    
040510       05 FILLER               PIC X(76)  VALUE SPACE.                    
040600     03   FILLER.                                                         
040700       05 FILLER               PIC X(10)  VALUE SPACE.                    
040800       05 FILLER               PIC X(13)  VALUE                           
040900          'Partner 1:   '.                                                
041000       05 FAIL-PARTNER-1       PIC X(43)  VALUE SPACE.                    
041010       05 FILLER               PIC X(34)  VALUE SPACE.                    
041100     03   FILLER.                                                         
041200       05 FILLER               PIC X(10)  VALUE SPACE.                    
041300       05 FILLER               PIC X(13)  VALUE                           
041400          'Partner 2:   '.                                                
041410       05 FAIL-PARTNER-2       PIC X(43)  VALUE SPACE.                    
041420       05 FILLER               PIC X(34)  VALUE SPACE.                    
041600     03   FILLER.                                                         
041700       05 FILLER               PIC X(10)  VALUE SPACE.                    
041800       05 FILLER               PIC X(13)  VALUE                           
041900          'Part no:     '.                                                
042000       05 FAIL-IDARTNR         PIC 9(8)   VALUE ZERO.                     
042100       05 FILLER               PIC X(69)  VALUE SPACE.                    
051000                                                                          
051100*         '12345678901234567890123456789012345678901234567890'.           
051200                                                                          
051300 01  FAIL-MAIL-TAB REDEFINES FAIL-MAIL-LINES.                             
051400     03  FAIL-LINE OCCURS 13 TIMES.                                       
051500         05 FILLER             PIC X(100).                                
051600                                                                          
051700                                                                          
073000 LINKAGE SECTION.                                                         
073100                                                                          
073200*01  -COPY W0009   -PRE MSG-                                              
073300                                                                          
073400*01  -COPY W0009   -PRE DISTRDOC-                                         
078800                                                                          
078900 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
082000 MAIN SECTION.                                                            
082100                                                                          
082200     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
085300                                                                          
085400     PERFORM IMS-GU-MSG                                                   
085500     IF SEGMENT-FOUND                                                     
085600                                                                          
085700       PERFORM A-INIT                                                     
085710       PERFORM B-TYPE-OF-ANSWER                                           
085720       EVALUATE TRUE                                                      
085800       WHEN MIC-PART                                                      
086000            PERFORM C-PART-MAIL                                           
086100       WHEN MIC-ORDER                                                     
086200            PERFORM D-ORDER-MAIL                                          
086210       WHEN MIC-ERROR                                                     
086220            PERFORM E-ERROR-MAIL                                          
086221       WHEN MIC-FAIL                                                      
086222            PERFORM F-FAIL-MAIL                                           
086230       WHEN OTHER                                                         
086240            CALL ABEND                                                    
086300       END-EVALUATE                                                       
086400                                                                          
086500*      PERFORM Z-FINIT                                                    
086600     END-IF                                                               
086700     MOVE ZERO TO RETURN-CODE                                             
086800     GOBACK                                                               
086900     .                                                                    
087000                                                                          
087100                                                                          
087200 A-INIT SECTION.                                                          
087210     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
087300                                                                          
088400*    MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I28601                     
088410*    MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-AREA                       
088411     IF MSG-SIGNON-USERID = 'PCCN616'                                     
088412* ONLY FOR TEST REASON                                                    
088413        MOVE MSG-IO-AREA(13:926) TO MID-W4I28601                          
088414     ELSE                                                                 
088420     MOVE MSG-IO-AREA TO MID-AREA                                         
088430     END-IF                                                               
088500*    call abend                                                           
089900     .                                                                    
090000                                                                          
090100                                                                          
090200 B-TYPE-OF-ANSWER SECTION.                                                
090210     MOVE 'B-TYPE-OF-ANSWER' TO CURRENT-SECTION                           
090300                                                                          
090301     IF MID-FLRESP = '1'                                                  
090302        IF (MID-BEPARTNR(1) NOT = SPACE AND                               
090303            MID-BEPARTNR(1) NOT = ALL 'A')                                
090304          or                                                              
090305           (MID-BEPARTNR(2) NOT = SPACE AND                               
090306            MID-BEPARTNR(2) NOT = ALL 'A')                                
090307           MOVE 'O'        TO TYPE-OF-ANSWER                              
090308        ELSE                                                              
090309           IF MID-IDARTNR(1) > ZERO                                       
090310              MOVE 'P'           TO TYPE-OF-ANSWER                        
090311           ELSE                                                           
090312              MOVE 'F'     TO TYPE-OF-ANSWER                              
090313           END-IF                                                         
090314        END-IF                                                            
090315     ELSE                                                                 
090316        IF MID-FLSTATUS = '1'                                             
090317           MOVE 'E'     TO TYPE-OF-ANSWER                                 
090318        ELSE                                                              
090319           MOVE 'F'     TO TYPE-OF-ANSWER                                 
090320        END-IF                                                            
090330     END-IF                                                               
090331     .                                                                    
090332                                                                          
090333 C-PART-MAIL SECTION.                                                     
090334     MOVE 'BC-PART-MAIL    ' TO CURRENT-SECTION                           
090335                                                                          
090336     MOVE 'PARTBLOCK'   TO HDR-IDOUTTYPE                                  
090337     MOVE 'MIC'         TO HDR-IDOUTREC                                   
090338     MOVE 'MIC'         TO HDR-IDLIST                                     
090343                                                                          
090344     MOVE MID-IDDISTR  TO PART-IDDISTR                                    
090345     MOVE MID-IDKUNDNR TO PART-IDKUNDNR                                   
090346     MOVE MID-IDDC     TO PART-IDDC-1                                     
090347                          PART-IDDC-2                                     
090348                          PART-IDDC-3                                     
090349                          PART-IDDC-4                                     
090350                          PART-IDDC-5                                     
090351                          PART-IDDC-6                                     
090352                          PART-IDDC-7                                     
090353                                                                          
090358     PERFORM S21-SEND-OPEN                                                
090359     PERFORM S22-PUT-HEADER                                               
090360                                                                          
090361     MOVE 1 TO LINE-IX                                                    
090362     PERFORM UNTIL LINE-IX > PART-LINE-MAX                                
090363        IF LINE-IX = 4                                                    
090364           PERFORM CA-PUT-PARTNO-LINE                                     
090365        END-IF                                                            
090366        MOVE PART-LINE(LINE-IX) TO SEND-RAD                               
090367        PERFORM S25-PUT-LINE                                              
090368        ADD 1 TO LINE-IX                                                  
090369     END-PERFORM                                                          
090370     PERFORM S29-SEND-CLOSE                                               
090376     .                                                                    
090377                                                                          
090378 CA-PUT-PARTNO-LINE SECTION.                                              
090379     MOVE 'CA-PUT-PART-LINE' TO CURRENT-SECTION                           
090380                                                                          
090381     MOVE SPACE TO PART-PART-LINE                                         
090382     MOVE 1 TO PART-PART-IX                                               
090383     PERFORM UNTIL PART-PART-IX > PART-PART-MAX                           
090384        MOVE ZERO TO PART-IDARTNR (PART-PART-IX)                          
090385        ADD 1 TO PART-PART-IX                                             
090386     END-PERFORM                                                          
090387                                                                          
090394     MOVE 1 TO PART-IX                                                    
090395     MOVE 1 TO PART-PART-IX                                               
090396     PERFORM UNTIL PART-IX > PART-IX-MAX                                  
090397                OR MID-IDARTNR(PART-IX) = ZERO                            
090398                                                                          
090400       MOVE MID-IDARTNR(PART-IX) TO PART-IDARTNR(PART-PART-IX)            
090401       ADD 1 TO PART-PART-IX                                              
090402       IF PART-PART-IX > PART-PART-MAX                                    
090403          MOVE PART-PART-LINE TO SEND-RAD                                 
090404          PERFORM S25-PUT-LINE                                            
090406          MOVE 1 TO PART-PART-IX                                          
090407          PERFORM UNTIL PART-PART-IX > PART-PART-MAX                      
090408             MOVE ZERO TO PART-IDARTNR (PART-PART-IX)                     
090409             ADD 1 TO PART-PART-IX                                        
090410          END-PERFORM                                                     
090411          MOVE 1 TO PART-PART-IX                                          
090412       END-IF                                                             
090413       ADD 1 TO PART-IX                                                   
090414     END-PERFORM                                                          
090415                                                                          
090416     IF PART-IDARTNR(1) > SPACE                                           
090417        MOVE PART-PART-LINE TO SEND-RAD                                   
090418        PERFORM S25-PUT-LINE                                              
090419     END-IF                                                               
090420     .                                                                    
090421                                                                          
090422 D-ORDER-MAIL SECTION.                                                    
090423     MOVE 'D-ORDER-MAIL    ' TO CURRENT-SECTION                           
090424                                                                          
090425     MOVE 'ORDERBLOCK'  TO HDR-IDOUTTYPE                                  
090426     MOVE 'MIC'         TO HDR-IDOUTREC                                   
090427     MOVE 'MIC'         TO HDR-IDLIST                                     
090428                                                                          
090429     MOVE MID-IDDISTR  TO ORDER-IDDISTR                                   
090430     MOVE MID-IDKUNDNR TO ORDER-IDKUNDNR                                  
090431     MOVE MID-IDDC     TO ORDER-IDDC-1                                    
090432                          ORDER-IDDC-2                                    
090433                          ORDER-IDDC-3                                    
090434                          ORDER-IDDC-4                                    
090435                          ORDER-IDDC-5                                    
090436                          ORDER-IDDC-6                                    
090437                          ORDER-IDDC-7                                    
090438                                                                          
090439     PERFORM S21-SEND-OPEN                                                
090440     PERFORM S22-PUT-HEADER                                               
090441     MOVE 1 TO LINE-IX                                                    
090442     PERFORM UNTIL LINE-IX > ORDER-LINE-MAX                               
090443        MOVE ORDER-LINE(LINE-IX) TO SEND-RAD                              
090444        PERFORM S25-PUT-LINE                                              
090445        ADD 1 TO LINE-IX                                                  
090446     END-PERFORM                                                          
090447     PERFORM S29-SEND-CLOSE                                               
090448     .                                                                    
090449                                                                          
090450 E-ERROR-MAIL SECTION.                                                    
090451     MOVE 'E-ERROR-MAIL    ' TO CURRENT-SECTION                           
090452                                                                          
090453     MOVE 'ERRORBLOCK'  TO HDR-IDOUTTYPE                                  
090454     MOVE 'MIC'         TO HDR-IDOUTREC                                   
090455     MOVE 'MIC'         TO HDR-IDLIST                                     
090456                                                                          
090457     MOVE MID-IDDISTR  TO ERROR-IDDISTR                                   
090458     MOVE MID-IDKUNDNR TO ERROR-IDKUNDNR                                  
090459     MOVE MID-IDORDNR7 TO ERROR-IDORDNR7                                  
090460     MOVE MID-IDDC     TO ERROR-IDDC-1                                    
090461                          ERROR-IDDC-2                                    
090462                          ERROR-IDDC-3                                    
090463                          ERROR-IDDC-4                                    
090464                          ERROR-IDDC-5                                    
090465                          ERROR-IDDC-6                                    
090466                                                                          
090467     PERFORM S21-SEND-OPEN                                                
090468     PERFORM S22-PUT-HEADER                                               
090469     MOVE 1 TO LINE-IX                                                    
090470     PERFORM UNTIL LINE-IX > ERROR-LINE-MAX                               
090471        MOVE ERROR-LINE(LINE-IX) TO SEND-RAD                              
090472        PERFORM S25-PUT-LINE                                              
090473        ADD 1 TO LINE-IX                                                  
090474     END-PERFORM                                                          
090475     PERFORM S29-SEND-CLOSE                                               
090476     .                                                                    
090477                                                                          
090478 F-FAIL-MAIL SECTION.                                                     
090480     MOVE 'F-FAIL-MAIL     ' TO CURRENT-SECTION                           
090490                                                                          
090500     MOVE 'FAILBLOCK'    TO HDR-IDOUTTYPE                                 
090600     MOVE 'MIC'          TO HDR-IDOUTREC                                  
090700     MOVE 'MIC'          TO HDR-IDLIST                                    
090800                                                                          
090900     MOVE MID-IDDISTR    TO FAIL-IDDISTR                                  
091140     MOVE MID-IDKUNDNR   TO FAIL-IDKUNDNR                                 
091191     MOVE MID-IDORDNR7   TO FAIL-IDORDNR7                                 
091197     MOVE MID-IDDC       TO FAIL-IDDC                                     
091203     MOVE MID-TIMESTAMP  TO FAIL-TIMESTAMP                                
091209     MOVE MID-FLRESP     TO FAIL-FLRESP                                   
091215     MOVE MID-FLSTATUS   TO FAIL-FLSTATUS                                 
091221     MOVE MID-PARTNER(1) TO FAIL-PARTNER-1                                
091227     MOVE MID-PARTNER(2) TO FAIL-PARTNER-2                                
091233     MOVE MID-IDARTNR(1) TO FAIL-IDARTNR                                  
091900     PERFORM S21-SEND-OPEN                                                
092000     PERFORM S22-PUT-HEADER                                               
092100     MOVE 1 TO LINE-IX                                                    
092200     PERFORM UNTIL LINE-IX > FAIL-LINE-MAX                                
092300        MOVE FAIL-LINE(LINE-IX) TO SEND-RAD                               
092400        PERFORM S25-PUT-LINE                                              
092500        ADD 1 TO LINE-IX                                                  
092600     END-PERFORM                                                          
092700     PERFORM S29-SEND-CLOSE                                               
092800     .                                                                    
092900                                                                          
204700                                                                          
205100*Z-FINIT SECTION.                                                         
205400*                                                                         
205500*    PERFORM S29-SEND-CLOSE                                               
205600*                                                                         
205700*    .                                                                    
205800                                                                          
205900                                                                          
206000 S21-SEND-OPEN SECTION.                                                   
206100     MOVE 'S21-SEND-OPEN   '      TO CURRENT-SECTION                      
206200                                                                          
206300     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
206400     MOVE 'OPEN'                  TO SEND-KDFUNC                          
206500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
206600                                     SEND-OPEN-AREA                       
206700     IF SEND-KDRC > ZERO                                                  
206800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
206900       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
207000       DELIMITED BY SIZE INTO ERROR-TEXT-STYR                             
207100       DISPLAY ERROR-TEXT                                                 
207200       CALL FELLOG                                                        
207300     END-IF                                                               
207500     .                                                                    
207600                                                                          
207700                                                                          
207800 S22-PUT-HEADER SECTION.                                                  
207900     MOVE 'S22-PUT-HEADER  '      TO CURRENT-SECTION                      
208000                                                                          
208100     MOVE 1                       TO REQU-IDMSGVER                        
208200     MOVE 'R'                     TO REQU-KDPGMACT                        
208300     MOVE IDPGM                   TO REQU-IDUSER                          
208700     MOVE 'PUT'                   TO SEND-KDFUNC                          
208800     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
209000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
209100                                     SEND-KVDLEN                          
209200                                     HDR-AREA                             
209300     IF SEND-KDRC > ZERO                                                  
209400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
209500       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
209600       DELIMITED BY SIZE       INTO ERROR-TEXT-STYR                       
209700       DISPLAY ERROR-TEXT                                                 
209800       CALL FELLOG                                                        
209900     END-IF                                                               
210000     .                                                                    
210100                                                                          
210200                                                                          
210300 S25-PUT-LINE SECTION.                                                    
210400     MOVE 'S25-PUT-LINE    '      TO CURRENT-SECTION                      
210500                                                                          
210600     MOVE 'PUT'                   TO SEND-KDFUNC                          
210700     MOVE LENGTH OF SEND-RAD      TO SEND-KVDLEN                          
210900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
211000                                     SEND-KVDLEN                          
211100                                     SEND-RAD                             
211200     IF SEND-KDRC > ZERO                                                  
211300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
211400       STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                     
211500       DELIMITED BY SIZE       INTO ERROR-TEXT-STYR                       
211600       DISPLAY ERROR-TEXT                                                 
211700       CALL FELLOG                                                        
211800     END-IF                                                               
211900     .                                                                    
212000                                                                          
212100                                                                          
212200 S29-SEND-CLOSE SECTION.                                                  
212300     MOVE 'S29-SEND-CLOSE  '     TO CURRENT-SECTION                       
212400                                                                          
212600     MOVE 'CLOSE'                TO SEND-KDFUNC                           
212800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
212900     IF SEND-KDRC > 0                                                     
213000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
213100       STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                    
213200       DELIMITED BY SIZE       INTO ERROR-TEXT-STYR                       
213300       DISPLAY ERROR-TEXT                                                 
213400       CALL FELLOG                                                        
213500     END-IF                                                               
213700     .                                                                    
213800                                                                          
221200                                                                          
221300* --- IMS SEKTIONER ---                                                   
221400                                                                          
221500 IMS-GU-MSG SECTION.                                                      
221600                                                                          
222020     MOVE '  QC' TO GOOD-STATUSCODES                                      
222030     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
222040     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
222050     PERFORM IMS-STATUSCHECK                                              
222100     .                                                                    
222200                                                                          
222300 IMS-STATUSCHECK SECTION.                                                 
222400                                                                          
222500     SET STATUS-IX TO 1                                                   
222600     SEARCH GOOD-STATUS                                                   
222700       AT END                                                             
222800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
222900         DELIMITED BY SIZE INTO ERROR-TEXT                                
223000         CALL FELLOG                                                      
223100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
223200         CONTINUE                                                         
223300     END-SEARCH                                                           
223400     .                                                                    
