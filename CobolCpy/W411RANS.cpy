000100 01  RANS-W411RANS.                                                       
000200*                                 LÄNKAREA TILL W411RANS -                
000300*                                 KONTROLLERA RANSONERING                 
000400     03 RANS-BERADREF        PIC X(10).                                   
000500*                                 KUNDENS RADREFERENS                     
000600     03 RANS-FLEMBORD        PIC X.                                       
000700*                                 EMBALLAGEORDER ?                        
000800     03 RANS-FLFORBI         PIC X.                                       
000900*                                 FÖRBIORDERFLAGGA                        
001000     03 RANS-FLORDSPE        PIC X.                                       
001100*                                 SPECIALORDERFLAGGA                      
001200     03 RANS-FLOVRLEV        PIC X.                                       
001300*                                 ÖVERLEVERANS                            
001400     03 RANS-IDKAMPRF        PIC S9(7)           COMP-3.                  
001500*                                 KAMPANJREFERENS                         
001600     03 RANS-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RANS-IDLEVNR         PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 RANS-IDRFTAB         PIC X(3).                                    
002100*                                 RANSONERINGSFAKTORTABELL                
002200     03 RANS-TIRODAT         PIC S9(7)           COMP-3.                  
002300*                                 RESTORDERDATUM         (ÅÅMMDD)         
002400     03 RANS-KDORDBEH        PIC S9              COMP-3.                  
002500*                                 STATUSKOD ORDERBEHANDLING               
002600     03 RANS-KDORDKL         PIC S9              COMP-3.                  
002700*                                 ORDERKLASS                              
002800     03 RANS-KVBEART-Q       PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003000     03 RANS-KDTPOTYP        PIC S9              COMP-3.                  
003100*                                 TYP AV TIDPLANERAD ORDER                
003200     03 RANS-KDERS           PIC S9(3)           COMP-3.                  
003300*                                 ERSÄTTNINGSKOD                          
003400     03 RANS-KVLS            PIC S9(7)           COMP-3.                  
003500*                                 LAGERSALDO                              
003600     03 RANS-KVPB-SATS       PIC S9(6)V9(1)      COMP-3.                  
003700*                                 SATS-PERIODBEHOV                        
003800     03 RANS-KVPB-SEP        PIC S9(6)V9(1)      COMP-3.                  
003900*                                 SEPARAT PERIODBEHOV                     
004000     03 RANS-REDIRLEV        PIC S9V9(2)         COMP-3.                  
004100*                                 DIREKTLEVERANSANDEL                     
004200     03 RANS-KVRESS          PIC S9(7)           COMP-3.                  
004300*                                 RESERVERAT ANTAL ARTIKLAR               
004400     03 RANS-KVSPANT         PIC S9(7)           COMP-3.                  
004500*                                 SPÄRRAT ANTAL                           
004600     03 RANS-KVUTRS          PIC S9(7)           COMP-3.                  
004700*                                 UTREDNINGSSALDO                         
004800     03 RANS-TIDISPIN        PIC S9(7)           COMP-3.                  
004900*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005000     03 RANS-RERF-RAD-UT     PIC S9V9(4)         COMP-3.                  
005100*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
005200     03 RANS-RERF-ART-UT     PIC S9V9(4)         COMP-3.                  
005300*                                 RANSONERINGSFAKTOR ARTIKEL              
005400     03 RANS-SUTPO-PB-UT     PIC S9(7)           COMP-3.                  
005500*                                 TPO-KVANTITET, BEHOVSPÅVERKANDE         
005600     03 RANS-SUTPO-EJPB-UT   PIC S9(7)           COMP-3.                  
005700*                                 TPO-KVANTITET, EJ BEHOVSPÅVERKA         
005800*                                 NDE                                     
005900*** END OF VILMAII-COPY LENGTH= 88 BYTES                                  
