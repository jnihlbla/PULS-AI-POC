000100 01  CDCI-W200CDCI.                                                       
000200*                                 LINK AREA FOR W200CDCI                  
000300     03 CDCI-INDATA.                                                      
000400        05 CDCI-IDARTNR-IN   PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 CDCI-UTDATA.                                                      
000700        05 CDCI-BEFT         PIC S9(3)           COMP-3.                  
000800*                                 FÖRPACKNINGSTYP                         
000900        05 CDCI-IDARTNR-EMBQ0                                             
001000                             PIC S9(9)           COMP-3.                  
001100*                                 EMBALLAGEARTIKELNR FÖR Q0               
001200        05 CDCI-IDARTNR-EMBQ1                                             
001300                             PIC S9(9)           COMP-3.                  
001400*                                 EMBALLAGEARTIKELNR FÖR Q1               
001500        05 CDCI-IDARTNR-EMBQ2                                             
001600                             PIC S9(9)           COMP-3.                  
001700*                                 EMBALLAGEARTIKELNR FÖR Q2               
001800        05 CDCI-IDARTNR-EMBQ3                                             
001900                             PIC S9(9)           COMP-3.                  
002000*                                 EMBALLAGEARTIKELNR FÖR Q3               
002100        05 CDCI-IDPSN        PIC 9(3).                                    
002200*                                 PROPER SHIPPING NAME                    
002300        05 CDCI-KVQPACK-3    PIC S9(5)           COMP-3.                  
002400*                                 ANTAL I Q3 FÖRPACKNING                  
002500        05 CDCI-CALLOFF.                                                  
002600           07 CDCI-KVAVROP   PIC Z(6)9.                                   
002700*                                 AVROPSKVANTITET                         
002800           07 CDCI-TIAVROP-AVS                                            
002900                             PIC 9(4).                                    
003000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003100*                                 (ÅÅVV)                                  
003200        05 CDCI-VKART        PIC S9(7)           COMP-3.                  
003300*                                 ARTIKELVIKT (G)                         
003400        05 CDCI-VLARTNTO     PIC S9(8)V9(1)      COMP-3.                  
003500*                                 ARTIKELVOLYM (CM3)                      
003600        05 CDCI-ADLAGOMR     PIC S9(3)           COMP-3.                  
003700*                                 LAGEROMRÅDE                             
003800        05 CDCI-ADGANG       PIC S9(3)           COMP-3.                  
003900*                                 GÅNG                                    
004000        05 CDCI-ADPLATS      PIC S9(5)           COMP-3.                  
004100*                                 LAGERPLATSNUMMER                        
004200        05 CDCI-KDARTURS     PIC X(2).                                    
004300*                                 ARTIKELURSPRUNGSKOD                     
004400        05 CDCI-IDLEVNR-MFG  PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600        05 CDCI-IDLEVNR-SHIP PIC X(5).                                    
004700*                                 SKEPPANDE LEVERANTÖR                    
004800        05 CDCI-KVVECKOR-LT  PIC 9(3).                                    
004900*                                 ANTAL VECKOR LEDTID                     
005000        05 CDCI-KVDAGAR-TT   PIC S9(3)           COMP-3.                  
005100*                                 DAGAR TULL- OCH TRANSPORT-TID           
005200        05 CDCI-KVDAGAR-INLEV                                             
005300                             PIC S9(3)           COMP-3.                  
005400*                                 INLEVERANSTID     (ANTAL DAGAR)         
005500        05 CDCI-TIETA        PIC S9(5)           COMP-3.                  
005600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005700        05 CDCI-KDFARLIG     PIC S9              COMP-3.                  
005800*                                 KOD FÖR FARLIGT GODS                    
005900     03 CDCI-KDSVAR          PIC X.                                       
006000      88 CDCI-KDSVAR-OK      VALUE ' '.                                   
006100      88 CDCI-KDSVAR-FEL     VALUE 'F'.                                   
006200*                                                       KDSVAR-88         
006300*                                 SVARSKOD FRÅN SUBPROGRAM                
006400     03 CDCI-IDMSG-ERROR     PIC X(3).                                    
006500*                                 FELMEDDELANDE ID                        
006600     03 CDCI-IDELMT-ERROR    PIC X(16).                                   
006700*                                 DATAELEMENTIDENTITET                    
006800     03 CDCI-FEL-TEXT        PIC X(25).                                   
006900*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
