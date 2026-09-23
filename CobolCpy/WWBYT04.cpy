000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID FÖLJANDE TESTER FÖR          
000300*                            *** BYTESENHETER:                            
000400*                            ***  - PRODUKT       =                       
000500*                            ***    PRODUKTSLAG FÖR BYTES.                
000600*                            ***    14 = PV                               
000610*                            ***    16 = RADIO                            
000700*                            ***    24 = CARPAC                           
000800*                            ***    34 = RENAULT                          
001000*                            *************************************        
001100*                                                                         
001200 01  BYT04-KDPRODSL          PIC 9(3)      COMP-3.                        
001300*                                                                         
001400       88  BYT04-PRODUKT     VALUE  14  16  24  34  54.                   
001500*** END COPY WWBYT04CC0  LENGTH=2     OLD LENGTH=2                        
