000100 01  W479401.                                                             
000200*                                 OUTPUT FILE FROM W479400                
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 RECORD TYPE                             
000500     03 IDORDER              PIC S9(7)           COMP-3.                  
000600*                                 VOLVO PARTS ORDER NUMBER                
000700     03 IDGMTREF.                                                         
000800*                                 GOODS RECEIVER REFERENS                 
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRICT NUMBER                         
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 CUSTOMER NO                             
001300        05 IDKUNDRF-GRP.                                                  
001400*                                 CUSTOMER REFERENCE (ORDER ID)           
001500           07 IDKUNDRF       PIC X(10).                                   
001600*                                 CUSTOMER REFERENCE (ORDER ID)           
001700           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001800              09 IDORDNR5    PIC 9(5).                                    
001900*                                 ORDER NUMBER                            
002000              09 FILLER      PIC X(5).                                    
002100           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002200              09 IDORDNR7    PIC 9(7).                                    
002300*                                 ORDER NUMBER                            
002400              09 FILLER      PIC X(3).                                    
002500     03 IDSYSTEM             PIC X(4).                                    
002600*                                 VOLVO VCCS SYSTEM NUMBER                
002700     03 IDUSER               PIC X(8).                                    
002800*                                 USER SECURITY-IDENTITY                  
002900     03 KDORDKL              PIC S9              COMP-3.                  
003000*                                 ORDER CLASS                             
003100     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003200*                                 REGISTRATION DATE (YYMMDD)              
003300     03 IDDC                 PIC X(2).                                    
003400*                                 WAREHOUSE IDENTIFIER                    
003500*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
