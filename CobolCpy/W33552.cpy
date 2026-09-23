000100 01  W33552.                                                              
000200*                                 UNAMBIGIOS REPLACEMENT FOR AMC          
000300*                                  REC TYPE = 552                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 IDVTYP               PIC X.                                       
000700*                                 RECORD TYPE VERSION                     
000800     03 IDARTNR              PIC 9(9).                                    
000900*                                 PART NUMBER                             
001000     03 KDERS                PIC 9(2).                                    
001100*                                 SUPERSESSION CODE                       
001200     03 IDARTNR-TILLK        PIC 9(9).                                    
001300*                                 REPLACEMENT PART NO.                    
001400     03 DIERS-KVOT           PIC 9(3)V9(3).                               
001500*                                 QUOTIENT BETWEEN                        
001600*                                 DIERS-TILLK AND DIERS-ERS               
001700     03 DAREGDAT             PIC 9(8).                                    
001800*                                 REGISTRATION DATE (YYYYMMDD)            
001900     03 KDPRODSL             PIC 9(2).                                    
002000*                                 PRODUCT GROUP                           
002100     03 KDPRODSL-TILLK       PIC 9(2).                                    
002200*                                 TYPE OF ASSORTMENT OF SUPERSEDI         
002300*                                 NG PART                                 
002400     03 KDERS-TILLK          PIC 9(2).                                    
002500*                                 REPLACING SUPERSESSION CODE             
002600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
