000100 01  W513703.                                                             
000200*                                 COPYTEXT TILL MANAGEMENT ACCUMU         
000300*                                 LATED ADJUSTMENTS LDC WEB               
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIAARP               PIC 9(4).                                    
000900*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001000*                                 12 PER ÅR                               
001100     03 KDJUSTYP             PIC 9.                                       
001200*                                 JUSTERINGSTYP                           
001300     03 KVJUSTKV-TOT         PIC Z(6)9.                                   
001400*                                 JUSTERAD KVANTITET                      
001500     03 SUARTSTD-TOT         PIC Z(7)9.9(2).                              
001600*                                 SUMMA STANDARDPRIS RADVÄRDE             
001700     03 KVJUSTKV-NEG         PIC Z(6)9.                                   
001800*                                 JUSTERAD KVANTITET                      
001900     03 SUARTSTD-NEG         PIC Z(7)9.9(2).                              
002000*                                 SUMMA STANDARDPRIS RADVÄRDE             
002100     03 KVJUSTKV-POS         PIC Z(6)9.                                   
002200*                                 JUSTERAD KVANTITET                      
002300     03 SUARTSTD-POS         PIC Z(7)9.9(2).                              
002400*                                 SUMMA STANDARDPRIS RADVÄRDE             
002500     03 KVJUSTKV-ZERO        PIC Z(6)9.                                   
002600*                                 JUSTERAD KVANTITET                      
002700     03 IDLANDX2             PIC X(2).                                    
002800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002900     03 ADCITY               PIC X(20).                                   
003000*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
