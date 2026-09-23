000100 01  SEQA-WDA8A1.                                                         
000200*                                 RETUR SPÄRRAR                           
000300*                                 TEELMT                                  
000400*                                 FYSISK NYCKEL: WDA8A1KY                 
000500*                                 (TEELMT + IDELMT + BESORTRT)            
000600*                                 SEKUNDÄR NYCKEL: WDA8ASEQ               
000700*                                 (TEELMT + IDELMT)                       
000800     03 SEQA-TEELMT          PIC X(16).                                   
000900*                                 NAMN FÖR ETT DATAELEMENT                
001000*                                 NAME OF AN ITEM                         
001100     03 SEQA-IDELMT          PIC X(16).                                   
001200*                                 DATAELEMENTIDENTITET / VÄRDE            
001300*                                 VALUE OF AN ITEM                        
001400     03 SEQA-BESORTRT        PIC X(20).                                   
001500*                                 SORTIMENT FÖR RETURER                   
001600*                                 SORT ALLOWED FOR RETURNS                
001700*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
