000100 01  1112-WDGX1112.                                                       
000200*                                 ERSÄTTNING                              
000300*                                 MELLANLAGRING ROT-INFO                  
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                  (IDARTNR + LOWVALUE)                   
000600     03 1112-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 1112-LOWVALUE        PIC X(5).                                    
001000     03 1112-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARIDENTITET I RACF                
001200*                                 USER RACF-IDENTITY                      
001300     03 1112-TIUPPDAT        PIC S9(7)           COMP-3.                  
001400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001500*                                 UPDATING DATE     (YYMMDD)              
001600     03 1112-DIERS-ERS       PIC S9(4)V9(3)      COMP-3.                  
001700*                                 ERSATT ARTIKELANTAL                     
001800*                                 NUMBER OF SUPERSEDED                    
001900     03 1112-FLMSKERS        PIC X.                                       
002000*                                 MASKINELL ERSÄTTNING ?                  
002100     03 1112-FLPUB           PIC X.                                       
002200*                                 PUBLICERINGSKOD                         
002300     03 1112-KVKORT          PIC S9(3)           COMP-3.                  
002400*                                 ANTAL KORT (ERSÄTTNINGS-RADER)          
002500     03 1112-TEARTNOT        PIC X(40).                                   
002600*                                 ARTIKEL NOTERING                        
002700*                                 PARTS NOTIFY                            
002800*** END COPY WDGX1112C0  LENGTH=70                                        
