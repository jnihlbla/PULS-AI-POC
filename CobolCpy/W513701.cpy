000100 01  W513701.                                                             
000200*                                 COPYTEXT TILL ACCUMULATED ADJUS         
000300*                                 TMENTS LDC                              
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIRP                 PIC 9(2).                                    
000900*                                 REDOVISNINGSPERIOD                      
001000*                                 12 PER ÅR                               
001100     03 KDJUSTYP             PIC 9.                                       
001200*                                 JUSTERINGSTYP                           
001300     03 KVJUSTKV-TOT         PIC Z(6)9.                                   
001400*                                 JUSTERAD KVANTITET                      
001500     03 KVJUSTKV-NEG         PIC Z(6)9.                                   
001600*                                 JUSTERAD KVANTITET                      
001700     03 KVJUSTKV-POS         PIC Z(6)9.                                   
001800*                                 JUSTERAD KVANTITET                      
001900     03 KVJUSTKV-ZERO        PIC Z(6)9.                                   
002000*                                 JUSTERAD KVANTITET                      
002100*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
