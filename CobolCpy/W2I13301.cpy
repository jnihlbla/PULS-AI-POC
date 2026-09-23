000100 01  MID-W2I13301.                                                        
000200*                                 MID-COPYTEXT FÖR W2013300               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDANSK-FOM       PIC 9(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-IDANSK-TOM       PIC 9(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MID-IDPROJ-VALT      PIC X(4).                                    
001200*                                 PARTS PROJEKTIDENTITET                  
001300     03 MID-IDANSK-MIN       PIC 9(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MID-IDPROJ-MIN       PIC X(4).                                    
001600*                                 PARTS PROJEKTIDENTITET                  
001700     03 MID-FLPISK-MIN       PIC X.                                       
001800*                                 PISK ARTIKEL                            
001900     03 MID-TIFINLEV-MIN     PIC 9(6).                                    
002000*                                 PUBLICERINGSDATUM  (AAMMDD)             
002100     03 MID-IDAO-MIN         PIC X(10).                                   
002200*                                 ÄNDRINGSORDERNUMMER                     
002300     03 MID-PRISDEL.                                                      
002400*                                 RAPPORTERINGSFÄLT 2133                  
002500*                                 PRISDELEN                               
002600        05 MID-IDLEVNR       PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800        05 MID-IDPLANGR-LEV  PIC 9.                                       
002900*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
003000        05 MID-IDPLANGR-AG   PIC 9.                                       
003100*                                 PLANERINGSGRUPP ANSKAFFARE              
003200        05 MID-IDANSK        PIC 9(3).                                    
003300*                                 ANSKAFFARNUMMER                         
003400        05 MID-PRARTSTD      PIC X(10).                                   
003500*                                 ARTIKELSTANDARDPRIS                     
003600        05 MID-KDTIPPR       PIC X.                                       
003700*                                 TIPPAT PRIS KOD                         
003800        05 MID-IDFTG         PIC X(2).                                    
003900*                                 FÖRETAGSID EKONOM REDOVISNING           
004000        05 MID-IDLKTO        PIC X(7).                                    
004100*                                 LAGERKONTO (FFHHHUU)                    
004200        05 MID-IDLKTO-POS1-7 REDEFINES MID-IDLKTO.                        
004300*                                 RAPPORTERINGSFÄLT 2133                  
004400*                                 PRISDELEN-KONTOFÄLTET                   
004500           07 MID-FILLER     PIC X(2).                                    
004600           07 MID-IDLKTO-POS3-7                                           
004700                             PIC X(5).                                    
004800        05 MID-KVPB-C1       PIC X(8).                                    
004900*                                 PERIODBEHOV (PROGNOS)                   
005000        05 MID-TIPBLOCK      PIC X(6).                                    
005100*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
005200*                                 ÅMMDD                                   
005300        05 MID-FLMPB-C1      PIC X.                                       
005400*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
005500        05 MID-KDHF          PIC 9.                                       
005600*                                 HUVUDFÖRRÅDSMÄRKNING                    
005700        05 MID-RESLJUST-C1   PIC X(3).                                    
005800*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
005900        05 MID-IDANSK-NOT    PIC X(40).                                   
006000*                                 ARTIKEL NOTERING                        
006100        05 MID-IDINK         PIC X(4).                                    
006200*                                 INKÖPARNUMMER                           
006300     03 MID-KOPDEL.                                                       
006400*                                 RAPPORTERINGSFÄLT 2133                  
006500*                                 KÖPDELEN                                
006600        05 MID-KDKOPTYP      PIC X.                                       
006700*                                 KOD TYP AV INKÖP                        
006800        05 MID-TILEVBEG      PIC 9(6).                                    
006900*                                 DATUM NÄR LEVERANS BEGÄRS               
007000        05 MID-KVLEVBEG      PIC 9(7).                                    
007100*                                 BEGÄRT ANTAL ATT LEVERERAS              
007200        05 MID-TEANSINK      PIC X(50).                                   
007300*                                 NOTERING VID FÖRSTA KÖP                 
007400        05 MID-KVPROG        PIC 9(7).                                    
007500*                                 ÅRSPROGNOS                              
007600     03 MID-FLNYRAPP         PIC X.                                       
007700*                                 SKA NYKÖPSRAPPORT SKRIVAS.              
007800*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
