000010*** EDIT ALLOWED                                                          
000100 01  W271FSG.                                                             
000200*                                 TABELL FAKTORER ONORMAL FÖR-            
000300*                                 SÄLJNING. ANVÄNDS FÖR ATT AV-           
000400*                                 GÖRA OM ONORMAL OI I NÅGON              
000500*                                 PERIOD VID PROGNOSBERÄKNING.            
000510*                                 GRÄNSVÄRDET ANVÄNDS I PROGNOS-          
000520*                                 BERÄKNINGEN ISTÄLLET FÖR DEN            
000530*                                 FAKTISKA ORDERINGÅNGEN.                 
000600*                                 (A * PROGNOS) + B                       
000700*                                                                         
000800     03 DC21-PRISKLASSER.                                                 
000900        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
001000        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
001100        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
001200        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
001300        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
001400        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
001500        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002901                                                                          
002902     03 DC23-PRISKLASSER.                                                 
002903        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002904        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002905        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002906        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002907        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002908        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002909        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002910                                                                          
002911     03 DC24-PRISKLASSER.                                                 
002912        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002913        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002914        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002915        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002916        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002917        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002918        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002919                                                                          
002920     03 DC25-PRISKLASSER.                                                 
002921        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002922        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002923        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002924        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002925        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002926        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002927        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002928                                                                          
002929     03 DC26-PRISKLASSER.                                                 
002930        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002931        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002932        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002933        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002934        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002935        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002936        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002937                                                                          
002938     03 DC41-PRISKLASSER.                                                 
002939        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002940        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002941        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002942        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002943        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002944        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002945        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002946                                                                          
002947     03 DC42-PRISKLASSER.                                                 
002948        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002949        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002950        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002951        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002952        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002953        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002954        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002955                                                                          
002956     03 DC43-PRISKLASSER.                                                 
002957        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002958        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002959        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002960        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002961        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002962        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002963        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002964                                                                          
002965     03 DC51-PRISKLASSER.                                                 
002966        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002967        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002968        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002969        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002970        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002971        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002972        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002973                                                                          
002974     03 DC61-PRISKLASSER.                                                 
002975        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002976        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002977        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002978        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002979        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002980        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002981        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002982                                                                          
002983     03 DC62-PRISKLASSER.                                                 
002984        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002985        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002986        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002987        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002988        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002989        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002990        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002982                                                                          
002983     03 DC1A-PRISKLASSER.                                                 
002984        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002985        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002986        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002987        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002988        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002989        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002990        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002991                                                                          
002983     03 DC1B-PRISKLASSER.                                                 
002984        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002985        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002986        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002987        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002988        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002989        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002990        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002991                                                                          
002983     03 DC2A-PRISKLASSER.                                                 
002984        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002985        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002986        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002987        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002988        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002989        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002990        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002991                                                                          
002902     03 DC3A-PRISKLASSER.                                                 
002903        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
002904        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
002905        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
002906        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
002907        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002908        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002909        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002910                                                                          
002992 01  FILLER REDEFINES W271FSG.                                            
003000     03 DC-FSGFAKT         OCCURS 15.                                     
003100        05 DC-FAKT             OCCURS 7.                                  
003200           07 DC-PRARTSTD-MAX  PIC 9(7)V9(2).                             
003300           07 FILLER          PIC X.                                      
003400           07 A               PIC 9(2)V9.                                 
003500           07 B               PIC 9(2).                                   
003510                                                                          
003600*** END COPY W271FSG     LENGTH=315                                       
