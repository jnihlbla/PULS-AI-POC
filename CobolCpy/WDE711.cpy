000100 01  SKLI-WDE711.                                                         
000200*                                 SAMLINGSKOLLI REGISTER                  
000300*                                 SAMLINGSKOLLI SEGMENT                   
000400*                                 FYSISK NYCKEL: IDKOLLI-SAMP             
000500*                                                                         
000600     03 SKLI-IDKOLLI-SAMP    PIC S9(5)           COMP-3.                  
000700*                                 SAMPACKNINGSKOLLINUMMER                 
000800*                                 MIXED PACKING CASE NUMBER               
000900     03 SKLI-ADFLGEO         PIC X(3).                                    
001000*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001100*                                 GEOGRAPHIC AREA                         
001200     03 SKLI-ADFLOMR         PIC S9(3)           COMP-3.                  
001300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001400*                                 DELIVERY AREA                           
001500     03 SKLI-ADRUTNIV        PIC S9(3)           COMP-3.                  
001600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001700*                                 SQUARE/LEVEL IN DEL. AREA               
001800     03 SKLI-FLFARLIG        PIC X.                                       
001900*                                 FARLIGT GODS-FLAGGA                     
002000*                                 DENGEROUS GOODS FLAG                    
002100     03 SKLI-IDDC            PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 SKLI-IDTRPTNR        PIC S9(3)           COMP-3.                  
002500*                                 TRANSPORTIDENTITET                      
002600*                                 TRANSPORT IDENTITY                      
002700     03 SKLI-KDATGSKLI       PIC X.                                       
002800*                                 TYP AV ÅTGÄRD FÖR SAMLINGSKOLLI         
002900*                                 TYPE OF HANDLE OF MIX CASE              
003000     03 SKLI-KDSTASKLI       PIC X.                                       
003100*                                 STATUS SAMLINGSKOLLI                    
003200*                                 STATUS MIX CASE                         
003300     03 SKLI-KDKOLLI-SAMP    PIC X(8).                                    
003400*                                 KOD BÄRKOLLI FÖR SAMPACKNING            
003500*                                 CODE MIXED PACKING CASE                 
003600     03 SKLI-KVKOLLI-SAMP    PIC S9(5)           COMP-3.                  
003700*                                 ANTAL KOLLI I SAMLINGSKOLLI             
003800*                                 NO OF CASES IN A MIX CASE               
003900     03 SKLI-TIREGDAT        PIC S9(7)           COMP-3.                  
004000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004100*                                 REGISTRATION DATE (YYMMDD)              
004200     03 SKLI-VKKOLLIN-SAMP   PIC S9(6)V9(1)      COMP-3.                  
004300*                                 SAMLINGSKOLLI-VIKT-NETTO                
004400*                                 NET WEIGHT OF MIX CASE                  
004500     03 SKLI-VKKOLLIB-SAMP   PIC S9(6)V9(1)      COMP-3.                  
004600*                                 SAMLINGSKOLLI-VIKT-BRUTTO               
004700*                                 GROSS WEIGHT OF MIX CASE                
004800     03 SKLI-VLKOLLIB-SAMP   PIC S9(4)V9(3)      COMP-3.                  
004900*                                 SAMLINGSKOLLIVOLYM BRUTTO (M3)          
005000*                                 MIX CASE GROSS VOLUME (M3)              
005100     03 SKLI-KDFRAKT         OCCURS 3 TIMES                               
005200                             PIC S9(3)           COMP-3.                  
005300*                                 FRAKTSÄTT DC TILL KUND                  
005400*                                 FREIGHT CODE                            
005500*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
