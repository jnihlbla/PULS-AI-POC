000100 01  RIU-W461RIU0.                                                        
000200*                                 COMPL RECORD AT REPLACEMENT             
000300*                                 OF BACKORDERED PART.                    
000400*                                 TO IMPORTER   RECORD TYPE RIU           
000500     03 RIU-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIU-KDCLAGER         PIC 9.                                       
000800*                                 CENTRAL WAREHOUSE CODE                  
000900     03 RIU-IDARTNR          PIC 9(9).                                    
001000*                                 PART NUMBER                             
001100     03 RIU-REKSIFFR         PIC 9.                                       
001200*                                 PART NO CHECK DIGIT                     
001300     03 RIU-IDLOPNRE         PIC 9(3).                                    
001400*                                 SEQUENCE NUMBER FOR EACH SUPER-         
001500*                                 SESSION                                 
001600     03 RIU-IDKORTNR         PIC 9(2).                                    
001700*                                 SEQUENCE NUMBER FOR EACH RECORD         
001800*                                  IN A SUPERSESSION                      
001900     03 RIU-KDRO             PIC 9.                                       
002000*                                 BACK ORDER CODE ON INFORMATION          
002100*                                 TO THE VR-SYSTEM.                       
002200     03 FILLER               PIC X(60).                                   
002300*** END COPY W461RIU0C0  LENGTH=80                                        
