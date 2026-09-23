000100 01  RYB-WDGZRYB.                                                         
000200*                                 RYB                                     
000300*                                 SKAPAS VID BACKNING AV BIPAC-           
000400*                                 KADE RO/TPO-RADER I ORDERKÖN            
000500*                                 OCH VID BEDÖMNING AV TPO:ER.            
000600*                                 ANVÄNDS VID TRANSAKTIONSSKA-            
000700*                                 PANDE TILL ÖVRIGA SYSTEM.               
000800     03 RYB-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RYB-BERADREF         PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 RYB-BEVOLREF         PIC X(10).                                   
001300*                                 VOLVO REFERENS                          
001400     03 RYB-FLTILLK          PIC X.                                       
001500*                                 TILLKOMMANDE ARTIKEL ?                  
001600     03 RYB-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RYB-IDDISTR          PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000     03 RYB-IDKUNDNR         PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200     03 RYB-IDKUNDRF         PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400     03 RYB-IDKUNDRF-RO      PIC X(10).                                   
002500*                                 KUND REF PÅ RO                          
002600     03 RYB-IDDC             PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 RYB-KDDSP            PIC S9              COMP-3.                  
002900*                                 PÅVERKAN PÅ DSP                         
003000     03 RYB-KDFAKTYP         PIC X.                                       
003100*                                 FAKTURATYP                              
003200     03 RYB-KDFRAKT          PIC S9(3)           COMP-3.                  
003300*                                 FRAKTSÄTT C1-C2 TILL KUND               
003400     03 RYB-KDLIDEL          PIC S9              COMP-3.                  
003500*                                 DEL AV LISTAN                           
003600     03 RYB-KDORDBEK         PIC 9(2).                                    
003700*                                 ORDERBEKRÄFTELSEKOD                     
003800     03 RYB-KDORDKL          PIC S9              COMP-3.                  
003900*                                 ORDERKLASS                              
004000     03 RYB-KDKVBRYT         PIC S9              COMP-3.                  
004100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004200     03 RYB-KDRO             PIC S9              COMP-3.                  
004300*                                 RESTORDERKOD PÅ INFORMATION             
004400*                                 TILL VR                                 
004500     03 RYB-KDVRINFO         PIC S9              COMP-3.                  
004600*                                 PÅVERKAN I VR/DSP SYSTEM                
004700     03 RYB-KVRO             PIC S9(7)           COMP-3.                  
004800*                                 ANTAL RESTNOTERADE ARTIKLAR             
004900     03 RYB-REKSIFFR         PIC S9              COMP-3.                  
005000*                                 KONTROLLSIFFRA                          
005100     03 RYB-TIDISPIN         PIC S9(7)           COMP-3.                  
005200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005300     03 RYB-TIORDREG         PIC S9(7)           COMP-3.                  
005400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005500     03 RYB-TIRODAT          PIC S9(7)           COMP-3.                  
005600*                                 RESTORDERDATUM         (ÅÅMMDD)         
005700*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
