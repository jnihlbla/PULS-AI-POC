000100 01  RYC-WDGZRYC.                                                         
000200*                                 RYC                                     
000300*                                 SKAPAS VID ANNULLATION/FÖR-             
000400*                                 ÄNDRING AV ORDERRAD I ORDERKÖN          
000500*                                 ANVÄNDS VID TRANSAKTIONS-               
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 RYC-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 RYC-BERADREF         PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100     03 RYC-BEVOLREF         PIC X(10).                                   
001200*                                 VOLVO REFERENS                          
001300     03 RYC-FLTILLK          PIC X.                                       
001400*                                 TILLKOMMANDE ARTIKEL ?                  
001500     03 RYC-IDARTNR          PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 RYC-IDDISTR          PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900     03 RYC-IDKUNDNR         PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100     03 RYC-IDKUNDRF         PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300     03 RYC-IDKUNDRF-RO      PIC X(10).                                   
002400*                                 KUND REF PÅ RO                          
002500     03 RYC-KDDSP            PIC S9              COMP-3.                  
002600*                                 PÅVERKAN PÅ DSP                         
002700     03 RYC-KDFAKTYP         PIC X.                                       
002800*                                 FAKTURATYP                              
002900     03 RYC-KDFRAKT          PIC S9(3)           COMP-3.                  
003000*                                 FRAKTSÄTT C1-C2 TILL KUND               
003100     03 RYC-KDORDBEK         PIC 9(2).                                    
003200*                                 ORDERBEKRÄFTELSEKOD                     
003300     03 RYC-KDORDKL          PIC S9              COMP-3.                  
003400*                                 ORDERKLASS                              
003500     03 RYC-KDKVBRYT         PIC S9              COMP-3.                  
003600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003700     03 RYC-KDVRINFO         PIC S9              COMP-3.                  
003800*                                 PÅVERKAN I VR/DSP SYSTEM                
003900     03 RYC-KDVRTPO          PIC S9              COMP-3.                  
004000*                                 KOD FÖR TPO:ER FRÅN VR                  
004100     03 RYC-KVANNANT         PIC S9(7)           COMP-3.                  
004200*                                 ANNULLERAT ANTAL ARTIKLAR               
004300     03 RYC-REKSIFFR         PIC S9              COMP-3.                  
004400*                                 KONTROLLSIFFRA                          
004500     03 RYC-TIORDREG         PIC S9(7)           COMP-3.                  
004600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004700     03 RYC-TIRODAT          PIC S9(7)           COMP-3.                  
004800*                                 RESTORDERDATUM         (ÅÅMMDD)         
004900     03 RYC-FILLER           PIC X.                                       
005000*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
