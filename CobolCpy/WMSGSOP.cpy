000100 01  MSGSOP-WMSGSOP.                                                      
000200*                                 TRANSACTION TO PROGRAM W00606           
000300*                                 TO ORDER A PROCESS IN SOP               
000400     03 MSGSOP-LL            PIC S9(4)           COMP                     
000500                             VALUE +534.                                  
000600*                                 LÄNGD PÅ DENNA TRANSAKTION              
000700*                                 LENGTH OF THIS TRANSACTION              
000800     03 MSGSOP-Z1-Z2         PIC S9(4)           COMP                     
000900                             VALUE ZEROS.                                 
001000*                                 POS 3-4 I RDW                           
001100*                                 POS 3-4 IN RDW                          
001200     03 MSGSOP-KDTRANS       PIC X(8)                                     
001300                             VALUE 'W0T606U '.                            
001400*                                 TRANSAKTION W0T606U                     
001500*                                 TRANSACTION W0T606U                     
001600     03 MSGSOP-IDTRANS       PIC X(4)                                     
001700                             VALUE SPACES.                                
001800*                                 BILDNUMMER                              
001900*                                 SCREEN NUMBER                           
002000     03 MSGSOP-KDMFSFOR      PIC X                                        
002100                             VALUE SPACE.                                 
002200*                                 TYP AV MFS-FORMAT                       
002300*                                 1 = W-FORMAT  2 = N-FORMAT              
002400*                                 TYPE OF MFS FORMAT                      
002500     03 MSGSOP-W0I60601.                                                  
002600*                                 MID-COPYTEXT FÖR W0060600               
002700        05 MSGSOP-IDPROCESS  PIC X(10)                                    
002800                             VALUE SPACES.                                
002900*                                 PROCESSNAMN                             
003000*                                 PROCESS NAME                            
003100        05 MSGSOP-KDSOPFUNK  PIC X                                        
003200                             VALUE SPACE.                                 
003300*                                 FUNKTIONSTYP TILL SOP PROGRAM           
003400*                                 ACTION TYPE FOR SOP PROGRAM             
003500        05 MSGSOP-TIORDDAT   PIC 9(6)                                     
003600                             VALUE ZEROS.                                 
003700*                                 ORDERDATUM                              
003800        05 MSGSOP-TESYMBV    PIC X(500)                                   
003900                             VALUE SPACES.                                
004000*** END OF VILMAII-COPY LENGTH= 534 BYTES                                 
