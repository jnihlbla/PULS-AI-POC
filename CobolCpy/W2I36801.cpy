000100 01  MID-W2I36801.                                                        
000200*                                 MID-COPYTEXT FÖR W2036800               
000300     03 MID-IDDC-2368-IN     PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-2368-UT     PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-COPY-IDDC        PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-KVOT             PIC X(7).                                    
001000*                                 ANTAL ORDERTRÄFF                        
001100     03 MID-KVPB-LIM-HF      PIC X(8).                                    
001200*                                 GRÄNS PERIODBEHOV HÖG FREKVENT          
001300     03 MID-KVPB-LIM-LF      PIC X(8).                                    
001400*                                 GRÄNS PERIODBEHOV LÅG FREKVENT          
001500     03 MID-GRP              OCCURS 12 TIMES.                             
001600        05 MID-SELECT        PIC X.                                       
001700        05 MID-IDPERSON-BUY  PIC 9(3).                                    
001800*                                 PERSONKOD REFILLANSVARIG                
001900        05 MID-IDREFTAB-LF   PIC X.                                       
002000*                                 ID REFILLTABELL LÅG FREKVENT            
002100        05 MID-IDREFTAB-HF   PIC X.                                       
002200*                                 ID REFILLTABELL HÖG FREKVENT            
002300     03 MID-NEW-IDPERSON-BUY PIC 9(3).                                    
002400*                                 PERSONKOD REFILLANSVARIG                
002500     03 MID-NEW-BEBUYER      PIC X(35).                                   
002600*                                  BUYER BENÄMNING                        
002700     03 MID-NEW-IDREFTAB-LF  PIC X.                                       
002800*                                 ID REFILLTABELL LÅG FREKVENT            
002900     03 MID-NEW-IDREFTAB-HF  PIC X.                                       
003000*                                 ID REFILLTABELL HÖG FREKVENT            
003100*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
