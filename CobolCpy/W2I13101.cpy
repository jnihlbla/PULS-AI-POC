000100 01  W2I13101.                                                            
000200*                                 COPYTEXT FÖR MID W2I13101               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDARTNR-UT           PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 IDLEVNR-FRAM-UT      PIC X(5).                                    
000800*                                 FRAMTIDA LEVERANTÖRNUMMER               
000900     03 IDLEVNR-FRAM-IN      PIC X(5).                                    
001000*                                 FRAMTIDA LEVERANTÖRNUMMER               
001100     03 IDLEVNR-SHIP-FRAM-UT PIC X(5).                                    
001200*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
001300     03 IDLEVNR-SHIP-FRAM-IN PIC X(5).                                    
001400*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
001500     03 IDANSK-UT            PIC X(3).                                    
001600*                                 ANSKAFFARNUMMER                         
001700     03 IDANSK-IN            PIC X(3).                                    
001800*                                 ANSKAFFARNUMMER                         
001900     03 IDPLANGR-LEV-IN      PIC X.                                       
002000*                                 PLANERINGSGRUPP                         
002100     03 KVSPANT-C1-IN        PIC X(7).                                    
002200*                                 SPÄRRAT ANTAL                           
002300     03 KVSPANT-C2-IN        PIC X(7).                                    
002400*                                 SPÄRRAT ANTAL                           
002500     03 IDPLANGR-AG-UT       PIC X.                                       
002600*                                 PLANERINGSGRUPP                         
002700     03 IDPLANGR-AG-IN       PIC X.                                       
002800*                                 PLANERINGSGRUPP                         
002900     03 FLREFILL-IN          PIC X.                                       
003000*                                 REFILLARTIKEL                           
003100     03 TIREFSTO-IN          PIC X(6).                                    
003200*                                 BEORDRINGSSTOPPAD T.OM.                 
003300     03 RESLJUST-C1-UT       PIC X(3).                                    
003400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
003500     03 RESLJUST-C2-UT       PIC X(3).                                    
003600*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
003700     03 KVVECKOR-LT-IN       PIC X(3).                                    
003800*                                 ANTAL VECKOR LEDTID                     
003900     03 FLMANLT-IN           PIC X.                                       
004000*                                 MANUELLT SATT LEDTID ?                  
004100     03 RESLJUST-C1-IN       PIC X(3).                                    
004200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
004300     03 RESLJUST-C2-IN       PIC X(3).                                    
004400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
004500     03 TISLJUST-C1-UT       PIC X(4).                                    
004600*                                 VECKA DÅ JUSTERING AV SÄKER-            
004700*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
004800     03 TISLJUST-C2-UT       PIC X(4).                                    
004900*                                 VECKA DÅ JUSTERING AV SÄKER-            
005000*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
005100     03 KVVECKOR-AT-IN       PIC X(3).                                    
005200*                                 ANTAL VECKOR ANSKAFFNINGSTID            
005300     03 FLMANAT-IN           PIC X.                                       
005400*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
005500     03 TISLJUST-C1-IN       PIC X(4).                                    
005600*                                 VECKA DÅ JUSTERING AV SÄKER-            
005700*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
005800     03 TISLJUST-C2-IN       PIC X(4).                                    
005900*                                 VECKA DÅ JUSTERING AV SÄKER-            
006000*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
006100     03 KVBK-IN              PIC X(7).                                    
006200*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
006300     03 FLMANBK-IN           PIC X.                                       
006400*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
006500     03 KVSLAGER-IN-C1       PIC X(7).                                    
006600*                                 SÄKERHETSLAGER                          
006700     03 KVSLAGER-IN-C2       PIC X(7).                                    
006800*                                 SÄKERHETSLAGER                          
006900     03 KVQ-IN               PIC X(7).                                    
007000*                                 EKONOMISK HEMTAGNINGSKVANTITET          
007100     03 FLMANQ-IN            PIC X.                                       
007200*                                 MANUELL HEMTAGNINGSKVANTITET            
007300     03 KVULOAD-IN           PIC 9(7).                                    
007400*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
007500     03 KVQ-JUST-IN          PIC X(7).                                    
007600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
007700     03 TIQJUST-IN           PIC X(4).                                    
007800*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
007900     03 KVPALL-IN            PIC X(7).                                    
008000*                                 ANTAL I PALL                            
008100*** END OF VILMAII-COPY LENGTH= 159 BYTES                                 
