000100 01  B22-WL019322.                                                        
000200*                                 COPYTEXT FOR BOLLA PARZIALE OCH         
000300*                                  TOTALE LINE-DATA                       
000400     03 B22-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 B22-IDORDNR          PIC Z(4)9.                                   
000700*                                 ORDERNUMMER UTG≈R PD90                  
000800     03 B22-TIORDREG         PIC 9(6).                                    
000900*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
001000     03 B22-KVKOLLI          PIC Z(3)9.                                   
001100*                                 ANTAL KOLLI                             
001200     03 B22-VKORDBTO         PIC Z(5)9.9.                                 
001300*                                 ORDERVIKT BRUTTO (KG)                   
001400     03 B22-VLORDBTO         PIC Z(3)9.9(3).                              
001500*                                 ORDERVOLYM BRUTTO (M3)                  
001600     03 B22-IDTRPBOT         PIC X.                                       
001700*                                 BOLLA-DOKUMENT TECKEN                   
001800     03 B22-IDTRPBON         PIC X(7).                                    
001900*                                 BOLLA-DOKUMENT NUMMER                   
002000     03 B22-TEXT1            PIC X(25).                                   
002100     03 B22-TEXT2            PIC X(25).                                   
002200*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
