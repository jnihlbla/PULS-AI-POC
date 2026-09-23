000100 01  MID-W6I20301.                                                        
000200*                                 COPYTEXT FÖR MID W6I20301               
000300*                                                                         
000400     03 MID-IDKR-IN          PIC X(5).                                    
000500*                                 KONTROLLRAPPORT NUMMER                  
000600     03 MID-IDKR-UT          PIC X(5).                                    
000700*                                 KONTROLLRAPPORT NUMMER                  
000800     03 MID-INPUT.                                                        
000900*                                                                         
001000        05 MID-FLKRLFEL      PIC X.                                       
001100*                                 LEVERANTÖRSBEROENDE FEL                 
001200        05 MID-KDKRATG       PIC X.                                       
001300*                                 ÅTGÄRD BEGÄRD FÖR LEV.BER.FEL           
001400        05 MID-FLINKANS      PIC X.                                       
001500*                                 ÅTGÄRDSANSVAR INKÖP                     
001600        05 MID-ADATTENT      PIC X(40).                                   
001700*                                 ATTENTIONADRESS                         
001800        05 MID-FLKRTOVIR     PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000        05 MID-FLEJKNTRL     PIC X.                                       
002100*                                 KVALITET KONTROLL FLAGGA                
002200        05 MID-KVARBTID-IN   PIC X(4).                                    
002300*                                 ANTAL MANTIMMAR                         
002400        05 MID-TEKRSPEC-ATID PIC X(30).                                   
002500*                                 SPECIFIKATION ARBETSTID                 
002600        05 MID-SUOMK-IN      PIC 9(7).                                    
002700*                                 SUMMA OMKOSTNADER                       
002800        05 MID-TEKRSPEC-OMK  PIC X(30).                                   
002900*                                 SPECIFIKATION OMKOSTNADER               
003000        05 MID-SUMAT-IN      PIC X(10).                                   
003100*                                 MATERIALKOSTNAD                         
003200        05 MID-TEKRSPEC-MAT  PIC X(30).                                   
003300*                                 SPECIFIKATION MATERIALKOSTNAD           
003400        05 MID-FLKROMK       PIC X.                                       
003500*                                 OMKOSTNADER KLAR FÖR DEB AV LEV         
003600        05 MID-FLARBDEB      PIC X.                                       
003700*                                 ARB-KOST DEBITERAS J/N                  
003800        05 MID-FLKRLIM       PIC X.                                       
003900*                                 FLAGGA LÅGT VÄRDE                       
004000        05 MID-KDPERSON      PIC 9(3).                                    
004100*                                 PERSONKOD                               
004200        05 MID-IDKRATLF      PIC X(20).                                   
004300*                                 TELEFON TILL ANSVARIG                   
004400*                                                                         
004500        05 MID-FLKRGODK      PIC X.                                       
004600*                                 GOKDKÄND                                
004700        05 MID-FAX-TLX       PIC X(3).                                    
004800        05 MID-KDFAX         PIC X.                                       
004900     03 MID-FLANN            PIC X.                                       
005000     03 MID-KDMEMO           PIC X.                                       
005100*** END OF VILMAII-COPY LENGTH= 199 BYTES                                 
