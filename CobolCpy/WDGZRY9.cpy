000100 01  RY9-WDGZRY9.                                                         
000200*                                 RY9                                     
000300*                                 FÖRÄNDRINGAR PÅ RADDATABASEN            
000400*                                 SOM GJORTS PÅ IMSBILD                   
000500     03 RY9-IDPTYP           PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 RY9-BERADREF         PIC X(10).                                   
000800*                                 KUNDENS RADREFERENS                     
000900     03 RY9-BEVOLREF         PIC X(10).                                   
001000*                                 VOLVO REFERENS                          
001100     03 RY9-FLERS            PIC X.                                       
001200*                                 TILLKOMMANDE ARTIKEL ?                  
001300     03 RY9-FLNC             PIC X.                                       
001400*                                 NEW CONCEPT FLAGGA                      
001500     03 RY9-IDARTNR          PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 RY9-IDDIVORD         PIC S9(3)           COMP-3.                  
001800*                                 DIVERSEORDERNUMMER                      
001900     03 RY9-IDKUNDRF         PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100     03 RY9-IDLOPNR          PIC S9(3)           COMP-3.                  
002200*                                 LÖPNUMMER                               
002300     03 RY9-IDUSER           PIC X(8).                                    
002400*                                 ANVÄNDARENS SÄKERHETS ID                
002500     03 RY9-KDFAKTYP         PIC X.                                       
002600*                                 FAKTURATYP                              
002700     03 RY9-KDKVBRYT         PIC S9              COMP-3.                  
002800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002900     03 RY9-KDORDKL          PIC S9              COMP-3.                  
003000*                                 ORDERKLASS                              
003100     03 RY9-KVART            PIC S9(7)           COMP-3.                  
003200*                                 ANTAL ARTNR PER BRYTBEGREPP             
003300     03 RY9-KDDSP            PIC S9              COMP-3.                  
003400*                                 PÅVERKAN PÅ DSP                         
003500     03 RY9-KDRAPRIO         PIC S9(3)           COMP-3.                  
003600*                                 PRIORITETSKOD PÅ RADEN                  
003700     03 RY9-KDSTARAD         PIC X.                                       
003800*                                 RADSTATUSKOD                            
003900     03 RY9-KDTPOTYP         PIC S9              COMP-3.                  
004000*                                 TYP AV TIDPLANERAD ORDER                
004100     03 RY9-KDUART           PIC X.                                       
004200*                                 UNDANTAGSARTIKEL                        
004300     03 RY9-KDVRINFO         PIC S9              COMP-3.                  
004400*                                 PÅVERKAN I VR/DSP SYSTEM                
004500     03 RY9-KDVRTPO          PIC S9              COMP-3.                  
004600*                                 KOD FÖR TPO:ER FRÅN VR                  
004700     03 RY9-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELPRIS NETTO                       
004900     03 RY9-TIREGDAT         PIC S9(7)           COMP-3.                  
005000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005100     03 RY9-TIRES            PIC S9(7)           COMP-3.                  
005200*                                 RESERVATIONSDATUM                       
005300     03 RY9-TITPO            PIC S9(7)           COMP-3.                  
005400*                                 PLANERAD ORDERDATUM                     
005500     03 RY9-TIRODAT          PIC S9(7)           COMP-3.                  
005600*                                 RESTORDERDATUM         (ÅÅMMDD)         
005700     03 RY9-FLVR             PIC X.                                       
005800*                                 ANSLUTEN TILL VR-SYST                   
005900*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
