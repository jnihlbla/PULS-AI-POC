000100 01  W221L466.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT FÖRPACKNINGS                 
000400*                                 STRUKTUREN WDK2 OCH                     
000500*                                 ARTIKELREGISTRET WDK6                   
000600*                                                                         
000700     03 KDCALL               PIC S9(3)           COMP-3.                  
000800      88 LAS-EMB-INGAR-I     VALUE +466.                                  
000900*                                 ANROPSTYP       KDCALL-W221-002         
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 FLJANEJ-ANROP        PIC X.                                       
001300      88 ANROP-OK            VALUE 'J'.                                   
001400      88 ANROP-FEL           VALUE 'N'.                                   
001500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001600     03 IOAREA.                                                           
001700        05 IDARTNR-FORP      PIC S9(9)           COMP-3.                  
001800*                                 FÖRPACKAD ARTIKEL                       
001900        05 KDERS             PIC S9(3)           COMP-3.                  
002000*                                 ERSÄTTNINGSKOD                          
002100        05 KDEMBTYP          PIC S9(3)           COMP-3.                  
002200*                                 EMBALLAGETYP       KDEMBTYP-002         
002300        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
002400*                                 ANTAL KVANTITETFÖRPACKNINGAR            
002500        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
002600*                                 ANTAL KVANTITETFÖRPACKNINGAR            
002700        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
002800*                                 ANTAL KVANTITETFÖRPACKNINGAR            
002900        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
003000*                                 ANTAL KVANTITETFÖRPACKNINGAR            
003100        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
003200*                                 ANTAL KVANTITETFÖRPACKNINGAR            
003300        05 IDLEVNR           PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500        05 CLAGER            OCCURS 2 TIMES.                              
003600           07 KVPB-SEP       PIC S9(6)V9(1)      COMP-3.                  
003700*                                 SEPARAT PERIODBEHOV                     
003800*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
