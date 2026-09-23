000100 01  RYE-WDGZRYE.                                                         
000200*                                 RYE                                     
000300*                                 SKAPAS VID UTSKRIFT AV                  
000400*                                 EN ORDERRAD.                            
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 RYE-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 RYE-BERADREF         PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100     03 RYE-BEVOLREF         PIC X(10).                                   
001200*                                 VOLVO REFERENS                          
001300     03 RYE-FLDIRLEV         PIC X.                                       
001400*                                 DIREKTLEVERANS ?                        
001500     03 RYE-FLLSBOK          PIC X.                                       
001600*                                 LAGERAVBOKNING                          
001700     03 RYE-FLORDSPE         PIC X.                                       
001800*                                 SPECIALORDERFLAGGA                      
001900     03 RYE-FLTILLK          PIC X.                                       
002000*                                 TILLKOMMANDE ARTIKEL ?                  
002100     03 RYE-IDARTNR          PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 RYE-IDKUNDRF         PIC X(10).                                   
002400*                                 KUNDENS REFERENS (ORDERID)              
002500     03 RYE-IDKUNDRF-RO      PIC X(10).                                   
002600*                                 KUND REF PÅ RO                          
002700     03 RYE-IDDC             PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 RYE-KDDSP            PIC S9              COMP-3.                  
003000*                                 PÅVERKAN PÅ DSP                         
003100     03 RYE-KDFAKTYP         PIC X.                                       
003200*                                 FAKTURATYP                              
003300     03 RYE-KDKVBRYT         PIC S9              COMP-3.                  
003400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003500     03 RYE-KDORDBEK         PIC 9(2).                                    
003600*                                 ORDERBEKRÄFTELSEKOD                     
003700     03 RYE-KDORDING         PIC S9              COMP-3.                  
003800*                                 UPPDATERING ORDERINGÅNG                 
003900     03 RYE-KDPRODSL         PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTSLAG                             
004100     03 RYE-KDVRINFO         PIC S9              COMP-3.                  
004200*                                 PÅVERKAN I VR/DSP SYSTEM                
004300     03 RYE-KDVRTPO          PIC S9              COMP-3.                  
004400*                                 KOD FÖR TPO:ER FRÅN VR                  
004500     03 RYE-KVAVBART         PIC S9(7)           COMP-3.                  
004600*                                 AVBOKAT ANTAL ARTIKLAR                  
004700     03 RYE-KVBEART-Q        PIC S9(7)           COMP-3.                  
004800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004900     03 RYE-KVRO             PIC S9(7)           COMP-3.                  
005000*                                 ANTAL RESTNOTERADE ARTIKLAR             
005100     03 RYE-REKSIFFR         PIC S9              COMP-3.                  
005200*                                 KONTROLLSIFFRA                          
005300     03 RYE-TIDISPIN         PIC S9(7)           COMP-3.                  
005400*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005500     03 RYE-TIORDREG         PIC S9(7)           COMP-3.                  
005600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005700     03 RYE-TIRODAT          PIC S9(7)           COMP-3.                  
005800*                                 RESTORDERDATUM         (ÅÅMMDD)         
005900*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
