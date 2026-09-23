000100 01  BLOC-W221BLOC.                                                       
000200*                                 LÄNKAREA TILL W221BLOC -                
000300*                                 FLYTTA AVROP WDD905 PGA                 
000400*                                 BLOCKAD SHIP-LEVERANTÖR                 
000500*                                 (SE BILD 2149)                          
000600*                                                                         
000700*                                 KDSVAR = SPACE  FLYTT OK                
000800*                                 KDSVAR = NEJ FLYTT EJ OK                
000900*                                                                         
001000     03 BLOC-IDPGM           PIC X(8).                                    
001100*                                 PROGRAM IDENTITET                       
001200     03 BLOC-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 BLOC-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 BLOC-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 BLOC-IDLEVNR-SHIP    PIC X(5).                                    
001900*                                 SKEPPANDE LEVERANTÖR                    
002000     03 BLOC-IDLANDX2-SHIP   PIC X(2).                                    
002100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002200     03 BLOC-IDANSK          PIC S9(3)           COMP-3.                  
002300*                                 ANSKAFFARNUMMER                         
002400     03 BLOC-KDAVROP         PIC S9              COMP-3.                  
002500*                                 AVROPSKOD                               
002600     03 BLOC-KVQ             PIC S9(7)           COMP-3.                  
002700*                                 EKONOMISK HEMTAGNINGSKVANTITET          
002800     03 BLOC-KVPALL          PIC S9(7)           COMP-3.                  
002900*                                 ANTAL I PALL                            
003000     03 BLOC-KVULOAD         PIC S9(7)           COMP-3.                  
003100*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
003200     03 BLOC-TIAAMMDD-SPECST PIC S9(7)           COMP-3.                  
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400     03 BLOC-TIAAMMDD-FT     PIC S9(7)           COMP-3.                  
003500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003600     03 BLOC-KVDAGAR-TT      PIC S9(3)           COMP-3.                  
003700*                                 DAGAR TULL- OCH TRANSPORT-TID           
003800     03 BLOC-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
003900*                                 INLEVERANSTID     (ANTAL DAGAR)         
004000     03 BLOC-FLAGGA-DAGL-AVROP                                            
004100                             PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300     03 BLOC-TILEVDAG-DAGL   OCCURS 5 TIMES                               
004400                             PIC S9              COMP-3.                  
004500*                                 AVSÄNDNINGSDAG INOM VECKA               
004600     03 BLOC-INDATA          OCCURS 60 TIMES.                             
004700        05 BLOC-DAAVROP-AVS  PIC 9(6).                                    
004800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
004900*                                 (ÅÅÅÅVV)                                
005000        05 BLOC-TILEVDAG     PIC S9              COMP-3.                  
005100*                                 AVSÄNDNINGSDAG INOM VECKA               
005200        05 BLOC-KVAVROP      PIC S9(7)           COMP-3.                  
005300*                                 AVROPSKVANTITET                         
005400        05 BLOC-DAAVROP-FOM  PIC 9(6).                                    
005500*                                 STARTVECKA ÅTGÄRD AVROP                 
005600*                                 (ÅÅÅÅVV)                                
005700        05 BLOC-DAAVROP-TOM  PIC 9(6).                                    
005800*                                 SLUTVECKA ÅTGÄRD AVROP                  
005900*                                 (ÅÅÅÅVV)                                
006000        05 BLOC-DAAVROP-TFOM PIC 9(6).                                    
006100*                                 TIDIGARELAGD STARTVECKA AVROP           
006200*                                 (ÅÅÅÅVV)                                
006300     03 BLOC-KDSVAR          PIC X.                                       
006400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006500*** END OF VILMAII-COPY LENGTH= 1801 BYTES                                
