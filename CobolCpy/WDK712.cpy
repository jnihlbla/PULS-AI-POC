000100 01  LART-WDK712.                                                         
000200*                                 ARTIKELINFO NDC LÄNDER                  
000300*                                 GÄLLER ALLA DC INOM ETT LAND            
000400*                                 FYSISK NYCKEL: IDLANDX2                 
000500     03 LART-IDLANDX2        PIC X(2).                                    
000600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000700*                                 2-LETTER CODE FOR COUNTRY               
000800     03 LART-BEFT            PIC S9(3)           COMP-3.                  
000900*                                 FÖRPACKNINGSTYP                         
001000*                                 PACKAGING TYPE                          
001100     03 LART-DAPUBL          PIC 9(8).                                    
001200*                                 PUBLICERINGSDATUM PER ART/LAND          
001300*                                 DATE OF PUBLISHING PART/COUNTRY         
001400     03 LART-IDARTNR-EMBQ0   PIC S9(9)           COMP-3.                  
001500*                                 EMBALLAGEARTIKELNR FÖR Q0               
001600     03 LART-IDARTNR-EMBQ1   PIC S9(9)           COMP-3.                  
001700*                                 EMBALLAGEARTIKELNR FÖR Q1               
001800     03 LART-IDARTNR-EMBQ2   PIC S9(9)           COMP-3.                  
001900*                                 EMBALLAGEARTIKELNR FÖR Q2               
002000     03 LART-IDUSER-EMB      PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200*                                 USER SECURITY-IDENTITY                  
002300     03 LART-KDARTURS        PIC X(2).                                    
002400*                                 ARTIKELURSPRUNGSKOD                     
002500*                                 COUNTRY OF ORIGIN                       
002600     03 LART-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
002700*                                 INLEVERANSTID     (ANTAL DAGAR)         
002800     03 LART-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
002900*                                 ARTIKELNS SJÄLVKOSTNAD                  
003000*                                 COST OF SALES                           
003100     03 LART-PRMATRL         PIC S9(7)V9(2)      COMP-3.                  
003200*                                 FAST PRIS UNDER LÖPANDE ÅR              
003300*                                 MATERIAL PRICE FOR ACTUAL YEAR          
003400     03 LART-VKART           PIC S9(7)           COMP-3.                  
003500*                                 ARTIKELVIKT (G)                         
003600*                                 PART WEIGHT (G)                         
003700     03 LART-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
003800*                                 ARTIKELVOLYM (CM3)                      
003900*                                 PART VOLUME    (CM3)                    
004000     03 LART-TIERSDAT-VIPS   PIC S9(5)           COMP-3.                  
004100*                                 DATUM NÄR ERS. INFO TILL VIPS           
004200*                                 SEND DATE OF SUPERS. TO VIPS            
004300     03 LART-TIUPPDAT-EMB    PIC S9(7)           COMP-3.                  
004400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004500*                                 UPDATING DATE     (YYMMDD)              
004600     03 LART-KDMATRPR        PIC X.                                       
004700*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
004800*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
004900     03 LART-KVQPACK-3       PIC S9(5)           COMP-3.                  
005000*                                 ANTAL I Q3 FÖRPACKNING                  
005100*                                 QUANTITY IN BULK PACK Q3                
005200     03 LART-IDPSN-DC        PIC 9(3).                                    
005300*                                 PROPER SHIPPING NAME PER XDC            
005400*                                 PROPER SHIPPING NAME XDC                
005500     03 LART-FLMSKUPD        PIC X.                                       
005600*                                 MASKINELL UPPDATERING J/N               
005700     03 LART-FLREFERAL       PIC X.                                       
005800*                                 REFERALARTIKEL I LANDET                 
005900*                                 REFERALPART IN COUNTRY                  
006000     03 LART-FILLER          PIC X.                                       
006100*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
