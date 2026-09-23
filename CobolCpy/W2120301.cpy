000100 01  BEST-W2120301.                                                       
000200*                                 DATA FÖR FÖRÄNDRING AV                  
000300*                                 BESTÄLLNINGSEGMENT (ARTC22)             
000400*                                 NYUPPLÄGG    = PTYP NBE                 
000500*                                 UPPDATERING  = PTYP UBE                 
000600*                                 BORTTAG      = PTYP BBE                 
000700     03 BEST-IDPTYP          PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 BEST-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 BEST-IDBEST          PIC S9(13)          COMP-3.                  
001200*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001300*                                 PPP   = (PREFIX) INKÖPARNR              
001400*                                 BBBBBB= BESTÄLLARNR                     
001500*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
001600     03 BEST-IDLEVNR-BEST    PIC X(5).                                    
001700*                                 LEVERANTÖR ENL. BESTÄLLNING             
001800     03 BEST-KDBEH-BEST      PIC S9              COMP-3.                  
001900*                                 BEHANDLINGSKOD BESTÄLLNING              
002000     03 BEST-KVBEST          PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL                          
002200     03 BEST-KVBEST-BEKR     PIC S9(7)           COMP-3.                  
002300*                                 BEKRÄFTAT BESTÄLLT ANTAL                
002400     03 BEST-TIBEST          PIC S9(7)           COMP-3.                  
002500*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
002600*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
