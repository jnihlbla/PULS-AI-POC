000100 01  TMP-WDD713.                                                          
000200*                                 TEMPORÄR REGELÄNDRING AV                
000300*                                 ERSÄTTNINGSREGLER FÖR ARTIKELN          
000400*                                 FYSISK NYCKEL: WDD713KY                 
000500*                                 (DAREGDA9 + TIKLOCK9)                   
000600     03 TMP-DAREGDAT-9KOMPL  PIC 9(8).                                    
000700*                                 DATUMETS 9-KOMPLEMENT                   
000800*                                 DATES 9-COMPLEMENT                      
000900     03 TMP-TIKLOCK-9KOMPL   PIC S9(9)           COMP-3.                  
001000*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001100*                                 TIME SAVED AS 9-COMPLEMENT              
001200     03 TMP-KDERSTMP         PIC X.                                       
001300*                                 TEMPORÄR ERSÄTTNINGSREGEL               
001400*                                 TEMPORARY SUPERSESSION RULE             
001500     03 TMP-IDUSER           PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 TMP-SULV             PIC S9(9)V9(2)      COMP-3.                  
001900*                                 LAGERVÄRDE                              
002000*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
