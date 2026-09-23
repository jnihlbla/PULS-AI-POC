000100 01  W612601.                                                             
000200*                                 COPYTEXT TILL REFILL FOLLOW UP          
000300*                                                                         
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 PTYP310-LINES        PIC Z(5)9.                                   
000900*                                 ANTAL                                   
001000     03 AK-LINES             PIC Z(5)9.                                   
001100*                                 ANTAL                                   
001200     03 BINNED-LINES         PIC Z(5)9.                                   
001300*                                 ANTAL                                   
001400     03 BINNED-PRIO          PIC Z(5)9.                                   
001500*                                 ANTAL                                   
001600     03 TIME-TOT             PIC Z(3)9.9.                                 
001700*                                 ANTAL DAGAR MED DECIMAL                 
001800     03 DAYS-PRIO            PIC Z(3)9.9.                                 
001900*                                 ANTAL DAGAR MED DECIMAL                 
002000     03 TIAAMMDD             PIC 9(6).                                    
002100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002200     03 KDREFTYP             PIC X.                                       
002300*                                 TYP AV REFILLORDER                      
002400*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
