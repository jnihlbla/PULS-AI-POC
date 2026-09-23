000100 01  W23301-CTX.                                                          
000200*                                                                         
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000600*                                 PRODUKTSLAG                             
000700     03 TIFINLV              PIC S9(5)           COMP-3.                  
000800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
000900     03 IDINK                PIC X(4).                                    
001000*                                 INKÖPARNUMMER                           
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 KDAVT                PIC S9              COMP-3.                  
001400*                                 AVTALSMÄRKNING                          
001500     03 KDHF                 PIC S9              COMP-3.                  
001600*                                 HUVUDFÖRRÅDSMÄRKNING                    
001700     03 KVDAGAR-INLEV        PIC S9(3)           COMP-3.                  
001800*                                 INLEVERANSTID     (ANTAL DAGAR)         
001900     03 KVDAGAR-TT           PIC S9(3)           COMP-3.                  
002000*                                 DAGAR TULL- OCH TRANSPORT-TID           
002100     03 KVLAAN               PIC S9(7)           COMP-3.                  
002200*                                 LÅNESALDO                               
002300     03 KVQ                  PIC S9(7)           COMP-3.                  
002400*                                 EKONOMISK HEMTAGNINGSKVANTITET          
002500     03 KVQ-JUST             PIC S9(7)           COMP-3.                  
002600*                                 NY EKON HEMTAGNINGSKVANTITET            
002700     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
002800*                                 ANTAL VECKOR FRYSNINGSTID               
002900     03 TIQJUST              PIC S9(5)           COMP-3.                  
003000*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
003100     03 KVPB-SEP             OCCURS 2 TIMES                               
003200                             PIC S9(6)V9(1)      COMP-3.                  
003300*                                 SEPARAT PERIODBEHOV                     
003400     03 REDIRLEV             OCCURS 2 TIMES                               
003500                             PIC S9V9(2)         COMP-3.                  
003600*                                 DIREKTLEVERANSANDEL                     
003700     03 KDERS                PIC S9(3)           COMP-3.                  
003800*                                 ERSÄTTNINGSKOD                          
003900     03 KVAKS                OCCURS 2 TIMES                               
004000                             PIC S9(7)           COMP-3.                  
004100*                                 ANKOMSTSALDO                            
004200     03 KVLS                 OCCURS 2 TIMES                               
004300                             PIC S9(7)           COMP-3.                  
004400*                                 LAGERSALDO                              
004500     03 KVRESS               OCCURS 2 TIMES                               
004600                             PIC S9(7)           COMP-3.                  
004700*                                 RESERVERAT ANTAL ARTIKLAR               
004800     03 KVOKS-BULK           OCCURS 2 TIMES                               
004900                             PIC S9(7)           COMP-3.                  
005000*                                 ORDERKÖSALDO, KLASS 2-4                 
005100     03 KVOKS-DAG            OCCURS 2 TIMES                               
005200                             PIC S9(7)           COMP-3.                  
005300*                                 ORDERKÖSALDO, KLASS 1                   
005400     03 KVOKS-VOR            OCCURS 2 TIMES                               
005500                             PIC S9(7)           COMP-3.                  
005600*                                 ORDERKÖSALDO, VOR                       
005700     03 KVROS                OCCURS 2 TIMES                               
005800                             PIC S9(7)           COMP-3.                  
005900*                                 RESTORDERSALDO                          
006000     03 KVSLAGER             OCCURS 2 TIMES                               
006100                             PIC S9(7)           COMP-3.                  
006200*                                 SÄKERHETSLAGER                          
006300     03 KVLS-SDC-OVER        PIC S9(7)           COMP-3.                  
006400*                                 LAGERSALDO                              
006500*** END OF VILMAII-COPY LENGTH= 124 BYTES                                 
