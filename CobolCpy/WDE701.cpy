000100 01  SKTV-WDE701.                                                         
000200*                                 SAMLINGSKOLLI REGISTER                  
000300*                                 TRANSPORT PER VO                        
000400*                                 FYSISK NYCKEL: WDE701KY:                
000500*                                 (IDTRPTNR + IDVO +                      
000600*                                  IDDC + KDFRAKT-SKTRP)                  
000700*                                                                         
000800     03 SKTV-IDTRPTNR        PIC S9(3)           COMP-3.                  
000900*                                 TRANSPORTIDENTITET                      
001000*                                 TRANSPORT IDENTITY                      
001100     03 SKTV-IDVO            PIC 9(2).                                    
001200*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
001300*                                 AREA OF OPERATIONS MIX CASES            
001400     03 SKTV-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SKTV-KDFRAKT-SKTRP   PIC S9(3)           COMP-3.                  
001800*                                 FK FÖR SAMLINGSKOLLI                    
001900*                                 FC FOR CONSOLIDATED CASES               
002000*** END OF VILMAII-COPY LENGTH= 8 BYTES                                   
