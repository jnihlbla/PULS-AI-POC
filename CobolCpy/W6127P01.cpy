000100 01  W6127P01.                                                            
000200*                                 COPYTEXT TILL SKROT MANAGEMENT          
000300*                                 FOLLOW UPP PERIOD LIST                  
000400*                                 SCRAPPING MANAGEMENT FOLLOW UPP         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 ADCITY               PIC X(20).                                   
000800     03 TIAAPP               PIC 9(4).                                    
000900*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
001000*                                 12 PER ÅR                               
001100*                                 NUMERA ÄR DETTA "PV-PERIOD"             
001200     03 KVANTAL-REF          PIC Z(5)9.                                   
001300*                                 ANTAL                                   
001400     03 SUARTSTD-REF         PIC Z(7)9.9(2).                              
001500*                                 SUMMA STANDARDPRIS RADVÄRDE             
001600     03 KVANTAL-INH          PIC Z(5)9.                                   
001700*                                 ANTAL                                   
001800     03 SUARTSTD-INH         PIC Z(7)9.9(2).                              
001900*                                 SUMMA STANDARDPRIS RADVÄRDE             
002000     03 KVANTAL-RET          PIC Z(5)9.                                   
002100*                                 ANTAL                                   
002200     03 SUARTSTD-RET         PIC Z(7)9.9(2).                              
002300*                                 SUMMA STANDARDPRIS RADVÄRDE             
002400     03 KVANTAL-QAL          PIC Z(5)9.                                   
002500*                                 ANTAL                                   
002600     03 SUARTSTD-QAL         PIC Z(7)9.9(2).                              
002700*                                 SUMMA STANDARDPRIS RADVÄRDE             
002800     03 KVANTAL-ECO          PIC Z(5)9.                                   
002900*                                 ANTAL                                   
003000     03 SUARTSTD-ECO         PIC Z(7)9.9(2).                              
003100*                                 SUMMA STANDARDPRIS RADVÄRDE             
003200     03 KVANTAL-DC           PIC Z(5)9.                                   
003300*                                 ANTAL                                   
003400     03 SUARTSTD-DC          PIC Z(7)9.9(2).                              
003500*                                 SUMMA STANDARDPRIS RADVÄRDE             
003600     03 KVANTAL-MIX          PIC Z(5)9.                                   
003700*                                 ANTAL                                   
003800     03 SUARTSTD-MIX         PIC -(9)9.9(2).                              
003900*                                 SUMMA STANDARDPRIS RADVÄRDE             
004000     03 KVANTAL-R34          PIC Z(5)9.                                   
004100*                                 ANTAL                                   
004200     03 SUARTSTD-R34         PIC Z(7)9.9(2).                              
004300*                                 SUMMA STANDARDPRIS RADVÄRDE             
004400*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
