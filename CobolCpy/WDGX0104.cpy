000100 01  0104-WDGX0104.                                                       
000200     03 0104-ADDISPABS       PIC X(50).                                   
000300*                                 ABSTRACT ADDRESS                        
000400     03 0104-KDCOMTYPE       PIC X(10).                                   
000500*                                 COMMUNICATION METHOD                    
000600     03 0104-FLCOMMDIR       PIC X.                                       
000700*                                 DIRECTION OF COMMUNICATION              
000800     03 0104-FLCONN          PIC X.                                       
000900     03 0104-KVDAGAR-RESEND  PIC S9(3)           COMP-3.                  
001000*                                 NO. OF DAYS TO KEEP FOR RESEND          
001100     03 0104-METADATA        PIC X(600).                                  
001200     03 0104-API-METADATA-FILLER REDEFINES 0104-METADATA.                 
001300        05 0104-API-METADATA.                                             
001400           07 0104-IDAPI     PIC X(50).                                   
001500*                                 NAME OF THE API                         
001600           07 0104-IDUSERKEY PIC X(50).                                   
001700*                                 USER KEY                                
001800           07 0104-IDPATH-API                                             
001900                             PIC X(250).                                  
002000           07 0104-IDPTYP-API                                             
002100                             PIC X(6).                                    
002200           07 0104-IDPROXY   PIC X(32).                                   
002300*                                 KEY TO AUTHENTICATE WITH VOLVO          
002400*                                 CARS GATEWAY FOR API                    
002500           07 0104-IDAPPKEY  PIC X(75).                                   
002600*                                 KEY USED TO AUTHENTICATE TO DIF         
002700*                                 F APIS                                  
002800*                                 CAN BE MAPPED TO DIFFERENT HTTP         
002900*                                  HEADERS/QUERY PARM                     
003000*                                 DEPENDING ON THE API SPEC               
003100        05 FILLER            PIC X(137).                                  
003200     03 0104-MQ-METADATA-FILLER REDEFINES 0104-METADATA.                  
003300        05 0104-MQ-METADATA.                                              
003400           07 0104-IDINTEGRATION                                          
003500                             PIC X(10).                                   
003600*                                 INTEGRATION ID                          
003700           07 0104-IDCONTRACT-MQ                                          
003800                             PIC X(10).                                   
003900*                                 MQ CONTRACT ID                          
004000*                                                                         
004100           07 0104-IDMQOBJ   PIC X(48).                                   
004200*                                 MQ OBJECT NAME                          
004300           07 0104-KDMQOBJ   PIC X(12).                                   
004400*                                 MQ OBJECT TYPE                          
004500           07 0104-KDMQFMT   PIC X(8).                                    
004600*                                 MQ MESSAGE FORMAT                       
004700*                                   MQFMT-NONE       = (SPACES)           
004800*                                   MQFMT-STRING     = MQSTR              
004900           07 0104-KDDELIM-REC                                            
005000                             PIC X(8).                                    
005100*                                 RECORD DELIMITER                        
005200           07 0104-KVMSGLEN-MAX                                           
005300                             PIC 9(4).                                    
005400*                                 MAX MESSAGE LENGTH                      
005500*                                 ALSO CHECK KDMSGLEN                     
005600           07 0104-KDMSGLEN  PIC X(2).                                    
005700*                                 MESSAGE LENGTH UNIT                     
005800*                                 B  - BYTES                              
005900*                                 KB - KILO BYTES                         
006000*                                 MB - MEGA BYTES                         
006100*                                 GB - GIGA BYTES                         
006200           07 0104-FLMSGSPLIT                                             
006300                             PIC X.                                       
006400*                                 SPLIT LONG MESSAGES AT THE END          
006500*                                 OF A RECORD. USED TOGETHER WITH         
006600*                                 FLMSGGROUP. PRIMARILY FOR MQ            
006700*                                 TRANSMISSIONS WHERE MESSAGE LEN         
006800*                                 EXCEEDS MAX MSG LEN                     
006900           07 0104-FLMSGGROUP                                             
007000                             PIC X.                                       
007100*                                 SEND LONG MESSAGES AS MULTIPLE          
007200*                                 SMALL MESSAGES PART OF A GROUP          
007300*                                 USED TOGETHER WITH FLMSGSPLIT           
007400*                                 PRIMARILY FOR MQ TRANSMISSIONS          
007500*                                 WHERE MESSAGE LENGTH EXCEEDS            
007600*                                 MAX MSG LENGTH                          
007700           07 0104-IDQMGR    PIC X(4).                                    
007800*                                 MQ QUEUE MANAGER NAME                   
007900           07 0104-KDCCSID   PIC 9(5).                                    
008000*                                 CCSID OF A MESSAGE                      
008100*                                 EX: 278 - SWEDISH EBCDIC                
008200           07 0104-KDEOF     PIC X(8).                                    
008300*                                 END OF FILE INDICATOR                   
008400           07 0104-KDLOG-LEVEL                                            
008500                             PIC X.                                       
008600*                                 LOGGING LEVEL                           
008700*                                                                         
008800*                                 EX: T - TRACE                           
008900*                                     D - DEBUG                           
009000*                                     E - ERROR                           
009100*                                     W - WARN                            
009200*                                     I - INFO                            
009300*                                 BLANK - NONE                            
009400        05 FILLER            PIC X(478).                                  
009500*** END OF VILMAII-COPY LENGTH= 664 BYTES                                 
