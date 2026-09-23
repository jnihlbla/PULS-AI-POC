000100 01  W517391.                                                             
000200*                                 COPYTEXT TILL WEEKLY ADJUSTMENT         
000300*                                 S LDC                                   
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 TIVV                 PIC S9(3)           COMP-3.                  
000900*                                 VECKA  (VV)                             
001000     03 BEPRODSL             PIC X(24).                                   
001100     03 BETEXT               PIC X(5).                                    
001200     03 SUARTSTD-WEEK        PIC S9(5)           COMP-3.                  
001300*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001400     03 SUARTSTD-WEEKAVG     PIC S9(5)           COMP-3.                  
001500*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001600     03 SUARTSTD-TOT         PIC S9(5)           COMP-3.                  
001700*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
001800     03 REDIFF               PIC S9(9)V9(2)      COMP-3.                  
001900*                                 SUMMA STANDARDPRIS RADVÄRDE             
002000     03 SUARTSTD-DIFF        PIC S9(5)           COMP-3.                  
002100*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002200     03 REDIFF-LYEAR         PIC S9(5)           COMP-3.                  
002300*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002400*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
