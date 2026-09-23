000100 01  SBLK-WDK721.                                                         
000200*                                 LAGERINFORMATION                        
000300*                                 FYSISK NYCKEL KDSEGKEY                  
000400     03 SBLK-KDSEGKEY        PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 SBLK-DAORDSP         PIC 9(8).                                    
000800*                                 ANGER DATUM NÄR STATUS PÅ ORDER         
000900*                                 SPÄRRFLAGGA ÄNDRAS                      
001000*                                 YYYYMMDD                                
001100*                                 STATES THE DATE WHEN FLAG OF BL         
001200*                                 OCK ORDER IS CHANGED                    
001300     03 SBLK-DASPBULK        PIC 9(8).                                    
001400*                                 ANGER DATUM NÄR STATUS PÅ FLAGG         
001500*                                 A FÖR ATT                               
001600*                                 SPÄRRA BULKORDER ÄNDRAS                 
001700*                                 YYYYMMDD                                
001800*                                 STATES THE DATE WHEN FLAG OF BL         
001900*                                 OCK BULKORDER IS CHANGED                
002000     03 SBLK-DAORDSP-EJRO    PIC 9(8).                                    
002100*                                 ANGER DATUM NÄR FLAGGA FÖR ORDE         
002200*                                 RSPÄRR UTAN RESTNOTERING ÄNDRAS         
002300*                                 YYYYMMDD                                
002400*                                 STATES THE DATE WHEN FLAG OF OR         
002500*                                 DER BLOCK                               
002600*                                 WITHOUT BACKORDERING IS CHANGED         
002700     03 SBLK-IDUSER-ORDSP    PIC X(8).                                    
002800*                                 ANVÄNDAR-ID SPÄRRA ORDER                
002900*                                 USER ID ORDER BLOCK                     
003000     03 SBLK-IDUSER-SPBULK   PIC X(8).                                    
003100*                                 ANVÄNDAR-ID SPÄRRA BULKORDER            
003200*                                 USER ID BULKORDER BLOCK                 
003300     03 SBLK-IDUSER-ORDSP-EJRO                                            
003400                             PIC X(8).                                    
003500*                                 ANVÄNDAR-ID ORDERSPÄRR UTAN RES         
003600*                                 TNOTERING                               
003700*                                 USER ID ORDER BLOCK WITHOUT BAC         
003800*                                 KORDERING                               
003900     03 SBLK-TEARTNOT-ORDER  PIC X(70).                                   
004000*                                 ARTIKEL NOTERING ORDER                  
004100*                                 PART REMARKS NOTE ORDER                 
004200     03 SBLK-FILLER          PIC X(21).                                   
004300*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
