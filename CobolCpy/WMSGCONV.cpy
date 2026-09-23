000100 01  MSG-CONV-AREA.                                                       
000200*                                 LINK AREA TO PGM WMSGCONV               
000300*                                 IT CONVERTS A WEB MESSAGE               
000400*                                 NUMBER TO CORRESPONDING                 
000500*                                 TEXT AND A MESSAGE CODE TO USE          
000600*                                 IN NEW CONNECTIONS, ESPECIALLY          
000700*                                 API.                                    
000800*                                                                         
000900*                                 INPUT ARGUMENTS:                        
001000*                                  IDSPRAK      DESIRED LANGUAGE          
001100*                                  IDSYSTEM     USED TO TRANSLATE         
001200*                                               CORR WEB SYSTEM           
001300*                                               DEFAULT: WL01             
001400*                                  IDMSG-IN     MESSAGE NUMBER            
001500*                                  IDELMT       ITEM NAME                 
001600*                                                                         
001700*                                 OUTPUT ARGUMENTS:                       
001800*                                  IDMSG-OUT    OUTPUT MSG CODE           
001900*                                  MESSAGE      OUTPUT MESSAGE            
002000*                                                                         
002100     03 MSG-CONV-IDSPRAK     PIC X(2).                                    
002200*                                 2-STÄLLIG ISO SPRÅKKOD                  
002300*                                 2-LETTER ISO LANGUAGE CODE              
002400     03 MSG-CONV-IDSYSTEM    PIC X(4).                                    
002500*                                 VOLVO VCCS SYSTEMNUMMER                 
002600*                                 VOLVO VCCS SYSTEM NUMBER                
002700     03 MSG-CONV-IDMSG-IN    PIC X(3).                                    
002800*                                 MEDDELANDE NUMMER                       
002900*                                 MESSAGE NUMBER                          
003000*                                                                         
003100     03 MSG-CONV-IDELMT      PIC X(16).                                   
003200*                                 DATAELEMENTIDENTITET                    
003300*                                 DATA ITEM NAME                          
003400     03 MSG-CONV-IDMSG-OUT   PIC X(10).                                   
003500*                                 MEDDELANDE NUMMER                       
003600*                                 MESSAGE NUMBER                          
003700*                                                                         
003800     03 MSG-CONV-MESSAGE     PIC X(100).                                  
003900*                                 MEDDELANDE                              
004000*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
