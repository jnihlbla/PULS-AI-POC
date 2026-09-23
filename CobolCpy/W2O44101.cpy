000100 01  MOD-W2O44101.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDANSK-FROM-IN   PIC X(2).                                    
000700*                                 MFS DISPOSITION OF INPUT FIELD          
000800     03 MOD-IDANSK-FROM-UT   PIC X(3).                                    
000900*                                 PROCURER NO.                            
001000     03 MOD-IDANSK-TOM-IN    PIC X(2).                                    
001100*                                 MFS DISPOSITION OF INPUT FIELD          
001200     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
001300*                                 PROCURER NO.                            
001400     03 MOD-IDPROJ-IN        PIC X(2).                                    
001500*                                 MFS DISPOSITION OF INPUT FIELD          
001600     03 MOD-IDPROJ-UT        PIC X(4).                                    
001700*                                 PARTS PROJECT IDENTITY                  
001800     03 MOD-INFO-LINE        OCCURS 12 TIMES.                             
001900        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002000        05 MOD-KDCMD         PIC X.                                       
002100*                                 LINE UPDATE COMMAND                     
002200        05 MOD-IDARTNR       PIC Z(9).                                    
002300*                                 PART NUMBER                             
002400        05 MOD-IDDC          PIC X(2).                                    
002500*                                 WAREHOUSE IDENTIFIER                    
002600        05 MOD-IDPROJ        PIC X(4).                                    
002700*                                 PARTS PROJECT IDENTITY                  
002800        05 MOD-IDAO          PIC X(10).                                   
002900*                                 DESIGN CHANGE NOTICE                    
003000        05 MOD-TIFINLV       PIC 9(5).                                    
003100*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
003200        05 MOD-TIREGDAT      PIC 9(6).                                    
003300*                                 REGISTRATION DATE (YYMMDD)              
003400        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
003500*                                 FUNCTION GROUP                          
003600        05 MOD-IDANSK        PIC Z(2)9.                                   
003700*                                 PROCURER NO.                            
003800        05 MOD-NEW-IDANSK-ATTR                                            
003900                             PIC X(2).                                    
004000        05 MOD-NEW-IDANSK    PIC 9(3).                                    
004100*                                 PROCURER NO.                            
004200        05 MOD-FLCN          PIC X.                                       
004300     03 MOD-TEMFSINF         PIC X(55).                                   
004400*                                 INFORMATION MESSAGE                     
004500*** END OF VILMAII-COPY LENGTH= 739 BYTES                                 
