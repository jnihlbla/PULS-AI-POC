000100 01  W440011.                                                             
000200*                                 LEVERANSBESKEDS INFO                    
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 KDORDKL              PIC S9              COMP-3.                  
001600*                                 ORDERKLASS                              
001700     03 KDSTARAD             PIC X.                                       
001800*                                 RADSTATUSKOD                            
001900     03 KDRAPRIO             PIC S9(3)           COMP-3.                  
002000*                                 PRIORITETSKOD PÅ RADEN                  
002100     03 KVART                PIC S9(7)           COMP-3.                  
002200*                                 ANTAL ARTNR PER BRYTBEGREPP             
002300     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
002400*                                 ARTIKELPRIS NETTO                       
002500     03 TIRODAT              PIC S9(7)           COMP-3.                  
002600*                                 RESTORDERDATUM         (ÅÅMMDD)         
002700     03 TITPO                PIC S9(7)           COMP-3.                  
002800*                                 PLANERAD ORDERDATUM                     
002900     03 TILEVBSK             PIC S9(5)           COMP-3.                  
003000*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
003100     03 REROFORD             PIC S9(3)           COMP-3.                  
003200*                                 RESTORDERFÖRDELNINGSFAKTOR              
003300     03 BEART                PIC X(25).                                   
003400*                                 ARTIKELBENÄMNING                        
003500     03 TELEVBSK-EXT         PIC X(80).                                   
003600*                                 LEVERANSBESKED FÖR EXTERNT              
003700     03 TELEVBSK-EXT2        PIC X(80).                                   
003800*                                 LEVERANSBESKED FÖR EXTERNT              
003900*** END OF VILMAII-COPY LENGTH= 237 BYTES                                 
