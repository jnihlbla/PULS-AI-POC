000100*    CONSTRUCTED: 970415                                                  
000200*    NAME:        PV INKÖP, VIR II                                        
000300*                                                                         
000400*                                                                         
000500 01  PI30001.                                                             
000600*                    KEY FIELDS SAME IN PI30001-2-3-4 COPYTEXT            
000700   03 KEYS.                                                               
000800*                    *** PLANT ID                                         
000900     05  PLANT                                PIC X(1).                   
001000*                    *** PEPORT TYPE                                      
001100     05  REPTYPE                              PIC X(1).                   
001200*                    *** SERIAL NO REPORT                                 
001300     05  REPSERNO                             PIC X(5).                   
001400*                    ***                                                  
001500     05  FILLER   REDEFINES REPSERNO.                                     
001600       07  REPSERNO-POS1                      PIC X(1).                   
001700       07  REPSERNO-POS2-5                    PIC X(4).                   
001800*                                                                         
001900   03 THE-REST.                                                           
002000*                    *** RECORD TYPE (= 1)                                
002100     05  RECTYPE                              PIC X(1).                   
002200*                    *** STATUS                                           
002300     05  STATUS                               PIC X(1).                   
002400*                    *** SUPPLIER NUMBER                                  
002500     05  SUPPNO                               PIC X(5).                   
002600*                    *** REPORT-DATE YYYYMMDD                             
002700     05  REPDATE                              PIC X(8).                   
002800*                    *** PART NUMBER                                      
002900     05  PARTNO                               PIC X(9).                   
003000*                    *** MATERIAL CONTROLLER                              
003100     05  MCTRL                                PIC X(30).                  
003200*                    *** ISSUERS NAME                                     
003300     05  ISSNAME                              PIC X(30).                  
003400*                    *** ISSUERS TELEPHONE                                
003500     05  ISSTEL                               PIC X(16).                  
003600*                    *** ISSUERS FAX                                      
003700     05  ISSFAX                               PIC X(16).                  
003800*                    *** ISSUERS MEMO                                     
003900     05  ISSMEMO                              PIC X(16).                  
004000*                    *** DESCRIPTION                                      
004100     05  DESCRIPT                             PIC X(1320).                
004200*                    *** VA-NUMBER                                        
004300     05  VANR                                 PIC X(5).                   
004400*                    ***  ANDL COST CODE                                  
004500     05  HANDLCDE                             PIC X(1).                   
004600*                    *** HANDLING COST                                    
004700     05  HANDLCST                             PIC X(7).                   
004800*                    *** CURRENCY                                         
004900     05  CURRCDE                              PIC X(3).                   
005000*                    *** HANDLING HOURS                                   
005100     05  HANDLHOUR                            PIC X(3).                   
005200*                    *** DISPOSITION                                      
005300     05  DISPCDE                              PIC X(2).                   
005400*                    *** BATCHNUMBER                                      
005500     05  BATCHNO                              PIC X(20).                  
005600*                    *** SHIPPED QUANTITY                                 
005700     05  SHIPQTY                              PIC X(6).                   
005800*                    *** SHIPPING ID                                      
005900     05  SHIPID                               PIC X(9).                   
006000*                    *** SHIPPING DATE  YYYYMMDD                          
006100     05  SHIPDATE                             PIC X(8).                   
006200*                    *** DEVIATION CODE                                   
006300     05  DEVCDE                               PIC X(2).                   
006400*                    *** REJECTED QUANTITY                                
006500     05  REJQTY                               PIC X(6).                   
006600*                    *** ACTION VCC                                       
006700     05  RESPVCC                              PIC X(5).                   
006800*                    *** PARTNO IN PALLET 1                               
006900     05  PARTNOP1                             PIC X(9).                   
007000*                    *** QUANTITY UN PALLET 1                             
007100     05  QTYPALL1                             PIC X(6).                   
007200*                    *** PARTNO IN PALLET 2                               
007300     05  PARTNOP2                             PIC X(9).                   
007400*                    *** QUANTITY UN PALLET 2                             
007500     05  QTYPALL2                             PIC X(6).                   
007600*                    *** PARTNO IN PALLET 3                               
007700     05  PARTNOP3                             PIC X(9).                   
007800*                    *** QUANTITY UN PALLET 3                             
007900     05  QTYPALL3                             PIC X(6).                   
008000*                    *** CHASSI-NUMBER                                    
008100     05  CHASSNO                              PIC X(14).                  
008200*                    *** MILAGE                                           
008300     05  MILEAGE                              PIC X(6).                   
008400*                    *** PRODUCTION DATE                                  
008500     05  PRODDATE                             PIC X(8).                   
008600*                    *** VCCQ-NO                                          
008700     05  VCCQNO                               PIC X(9).                   
008800*                                                                         
008700     05  EMAILADR                             PIC X(64).                  
008800*                                                                         
008900  03 THE-END.                                                             
009000*                                                                         
009100     05  FILLER                               PIC X(318).                 
009200*                                                                         
009300*** END OF VILMAII-COPY LENGTH= 2000 OLD LENGTH= 2000                     
