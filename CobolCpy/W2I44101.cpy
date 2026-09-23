000100 01  MID-W2I44101.                                                        
000200     03 MID-IDANSK-FROM-IN   PIC X(3).                                    
000300*                                 PROCURER NO.                            
000400     03 MID-IDANSK-FROM-UT   PIC X(3).                                    
000500*                                 PROCURER NO.                            
000600     03 MID-IDANSK-TOM-IN    PIC X(3).                                    
000700*                                 PROCURER NO.                            
000800     03 MID-IDANSK-TOM-UT    PIC X(3).                                    
000900*                                 PROCURER NO.                            
001000     03 MID-IDPROJ-IN        PIC X(4).                                    
001100*                                 PARTS PROJECT IDENTITY                  
001200     03 MID-IDPROJ-UT        PIC X(4).                                    
001300*                                 PARTS PROJECT IDENTITY                  
001400     03 MID-INFO-LINE        OCCURS 12 TIMES.                             
001500        05 MID-KDCMD         PIC X.                                       
001600*                                 LINE UPDATE COMMAND                     
001700        05 MID-IDARTNR       PIC 9(9).                                    
001800*                                 PART NUMBER                             
001900        05 MID-IDDC          PIC X(2).                                    
002000*                                 WAREHOUSE IDENTIFIER                    
002100        05 MID-NEW-IDANSK    PIC 9(3).                                    
002200*                                 PROCURER NO.                            
002300*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
