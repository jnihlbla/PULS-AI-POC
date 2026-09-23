000100 01  W513702.                                                             
000200*                                 COPYTEXT TILL MANAGEMENT ACCUMU         
000300*                                 LATED ADJUSTMENTS LDC                   
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIRP                 PIC S9(2)           COMP-3.                  
000900*                                 REDOVISNINGSPERIOD                      
001000*                                 12 PER ÅR                               
001100     03 KDJUSTYP             PIC S9              COMP-3.                  
001200*                                 JUSTERINGSTYP                           
001300     03 KVJUSTKV-TOT         PIC S9(7)           COMP-3.                  
001400*                                 JUSTERAD KVANTITET                      
001500     03 SUARTSTD-TOT         PIC S9(9)V9(2)      COMP-3.                  
001600*                                 SUMMA STANDARDPRIS RADVÄRDE             
001700     03 KVJUSTKV-NEG         PIC S9(7)           COMP-3.                  
001800*                                 JUSTERAD KVANTITET                      
001900     03 SUARTSTD-NEG         PIC S9(9)V9(2)      COMP-3.                  
002000*                                 SUMMA STANDARDPRIS RADVÄRDE             
002100     03 KVJUSTKV-POS         PIC S9(7)           COMP-3.                  
002200*                                 JUSTERAD KVANTITET                      
002300     03 SUARTSTD-POS         PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA STANDARDPRIS RADVÄRDE             
002500     03 KVJUSTKV-ZERO        PIC S9(7)           COMP-3.                  
002600*                                 JUSTERAD KVANTITET                      
002700*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
