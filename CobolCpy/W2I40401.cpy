000100 01  MID-W2I40401.                                                        
000200     03 MID-IDARTNR-IN       PIC X(9).                                    
000300*                                 PART NUMBER                             
000400     03 MID-IDARTNR-UT       PIC X(9).                                    
000500*                                 PART NUMBER                             
000600     03 MID-IDDC-IN          PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 MID-IDDC-UT          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MID-INPUT.                                                        
001100        05 MID-KVPB-DC-IN    PIC X(8).                                    
001200*                                 PERIOD REQUIREMENTS                     
001300        05 MID-KVPBREOI-DC-IN                                             
001400                             PIC X(8).                                    
001500*                                 PERIOD REQUIREM. REFILLING OI           
001600        05 MID-TIREFMPB-IN   PIC 9(6).                                    
001700*                                 DATE MANUAL FORECAST REFILLING          
001800        05 MID-DAREFESC-IN   PIC 9(6).                                    
001900*                                 DATE MANUAL FORECAST REFILLING          
002000        05 MID-KVPB-PLAN-IN  PIC X(8).                                    
002100*                                 PERIOD REQUIREMENTS                     
002200        05 MID-DAPBPLAN-IN   PIC 9(6).                                    
002300*                                 DATE MANUAL FORECAST REFILLING          
002400*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
