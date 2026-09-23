000001*** EDIT ALLOWED                                                          
000002*****************************************************************         
000003* EMBALLAGE BENÄMNINGAR SOM SKRIVS PÅ FARLIG GODS DOKUMENTEN.   *         
000004* MHA AV IDSPRÅK HÄMTAS BENÄMNING MED RÄTT SPRÅK                *         
000005* BENÄMNINGSTABELLER FINNS FÖR BÅDE SINGULARIS (SUFIX 1)        *         
000006*                               OCH PLURALIS (SUFIX 2)          *         
000007*                                                               *         
000008*   PIC A(12) VALUE 'GB/TEXT PÅ 9 SPRÅK'.                       *         
000009*                     I     I                                   *         
000010*   SPRÅK       ------I     I                                   *         
000011*   EMB. BENÄMNING ---------I                                   *         
000012*                                                               *         
000013* OBS!!OBS!! VID TILLÄGG/BORTTAG ANPASSA OCCURS !!              *         
000014*                                                               *         
000015*****************************************************************         
000016                                                                          
000017 01  FEMB-FIBREBOARD-1.                                                   
000018                                                                          
000019   03   PIC X(29) VALUE 'DE/KISTE, PAPPE              '.                  
000020   03   PIC X(29) VALUE 'ES/CAJA DE CARTóN            '.                  
000021   03   PIC X(29) VALUE 'FI/PAHVISTA LAATIKKO         '.                  
000022   03   PIC X(29) VALUE 'FR/CAISSE                    '.                  
000023   03   PIC X(29) VALUE 'GB/FIBREBOARD BOX            '.                  
000024   03   PIC X(29) VALUE 'IT/SCATOLA CARTONE           '.                  
000025   03   PIC X(29) VALUE 'NL/KARTONNEN DOOS            '.                  
000026   03   PIC X(29) VALUE 'NO/PAPPKASSE                 '.                  
000027   03   PIC X(29) VALUE 'PL/SKRZYNIA KARTONOWA        '.                  
000028   03   PIC X(29) VALUE 'SE/PAPPLÅDA                  '.                  
000029                                                                          
000030 01  FILLER REDEFINES FEMB-FIBREBOARD-1.                                  
000031   03  FEMB-FRAD1  OCCURS 10 ASCENDING KEY IS IDSPRAK-F1                  
000040                             INDEXED BY F1-IX.                            
000050     05  IDSPRAK-F1          PIC X(2).                                    
000060     05  FILLER              PIC X(1).                                    
000070     05  TEEMB-F1            PIC X(26).                                   
000080                                                                          
000090 01  FEMB-FIBREBOARD-2.                                                   
000100                                                                          
000110   03   PIC X(29) VALUE 'DE/KISTEN, PAPPE             '.                  
000120   03   PIC X(29) VALUE 'ES/CAJAS DE CARTóN           '.                  
000130   03   PIC X(29) VALUE 'FI/PAHVILAATIKKOA            '.                  
000140   03   PIC X(29) VALUE 'FR/CAISSES                   '.                  
000150   03   PIC X(29) VALUE 'GB/FIBREBOARD BOXES          '.                  
000160   03   PIC X(29) VALUE 'IT/SCATOLA CARTONI           '.                  
000170   03   PIC X(29) VALUE 'NL/KARTONNEN DOZEN           '.                  
000171   03   PIC X(29) VALUE 'NO/PAPPKASSER                '.                  
000180   03   PIC X(29) VALUE 'PL/SKRZYNIA KARTONOWE        '.                  
000190   03   PIC X(29) VALUE 'SE/PAPPLÅDOR                 '.                  
000200                                                                          
000201 01  FILLER REDEFINES FEMB-FIBREBOARD-2.                                  
000202   03  FEMB-FRAD2  OCCURS 10 ASCENDING KEY IS IDSPRAK-F2                  
000203                             INDEXED BY F2-IX.                            
000204     05  IDSPRAK-F2          PIC X(2).                                    
000205     05  FILLER              PIC X(1).                                    
000206     05  TEEMB-F2            PIC X(26).                                   
000207                                                                          
000208 01  FEMB-PLASTICDRUM-1.                                                  
000209                                                                          
000210   03   PIC X(29) VALUE 'DE/FASS, KUNSTSTOFF          '.                  
000211   03   PIC X(29) VALUE 'ES/BIDóN DE PLáSTICO         '.                  
000212   03   PIC X(29) VALUE 'FI/MUOVITYNNYRI              '.                  
000213   03   PIC X(29) VALUE 'FR/FûT, PLASTIQUE            '.                  
000214   03   PIC X(29) VALUE 'GB/PLASTIC DRUM              '.                  
000215   03   PIC X(29) VALUE 'IT/FUSTO DI PLASTICA         '.                  
000216   03   PIC X(29) VALUE 'NL/KUNSTSTOF VAT             '.                  
000217   03   PIC X(29) VALUE 'NO/PLASTFAT                  '.                  
000218   03   PIC X(29) VALUE 'PL/BEBEN PLASTIKOWY          '.                  
000219   03   PIC X(29) VALUE 'SE/PLASTFAT                  '.                  
000220                                                                          
000221 01  FILLER REDEFINES FEMB-PLASTICDRUM-1.                                 
000230   03  FEMB-PRAD1  OCCURS 10 ASCENDING KEY IS IDSPRAK-P1                  
000240                             INDEXED BY P1-IX.                            
000250     05  IDSPRAK-P1          PIC X(2).                                    
000260     05  FILLER              PIC X(1).                                    
000270     05  TEEMB-P1            PIC X(26).                                   
000280                                                                          
000290 01  FEMB-PLASTICDRUM-2.                                                  
000300                                                                          
000400   03   PIC X(29) VALUE 'DE/FÄSSER, KUNSTSTOFF        '.                  
000500   03   PIC X(29) VALUE 'ES/BIDóNES DE PLáSTICO       '.                  
000600   03   PIC X(29) VALUE 'FI/MUOVITYNNYREIDEN          '.                  
000700   03   PIC X(29) VALUE 'FR/FûTS, PLASTIQUE           '.                  
000800   03   PIC X(29) VALUE 'GB/PLASTIC DRUMS             '.                  
000900   03   PIC X(29) VALUE 'IT/FURSTI DI PLASTICA        '.                  
001000   03   PIC X(29) VALUE 'NL/KUNSTOF VATEN             '.                  
001010   03   PIC X(29) VALUE 'NO/PLASTFAT                  '.                  
001100   03   PIC X(29) VALUE 'PL/BEBNY PLASTIKOWY          '.                  
001200   03   PIC X(29) VALUE 'SE/PLASTFAT                  '.                  
001300                                                                          
001400 01  FILLER REDEFINES FEMB-PLASTICDRUM-2.                                 
001500   03  FEMB-PRAD2  OCCURS 10 ASCENDING KEY IS IDSPRAK-P2                  
001510                             INDEXED BY P2-IX.                            
001520     05  IDSPRAK-P2          PIC X(2).                                    
001530     05  FILLER              PIC X(1).                                    
001540     05  TEEMB-P2            PIC X(26).                                   
001550                                                                          
001560 01  FEMB-STEELDRUM-1.                                                    
001570                                                                          
001580   03   PIC X(29) VALUE 'DE/FASS, STAHL               '.                  
001590   03   PIC X(29) VALUE 'ES/BIDóN DE ACERO            '.                  
001600   03   PIC X(29) VALUE 'FI/TERÄSTYNNYRI              '.                  
001700   03   PIC X(29) VALUE 'FR/FûT, ACIER                '.                  
001800   03   PIC X(29) VALUE 'GB/STEEL  DRUM               '.                  
001900   03   PIC X(29) VALUE 'IT/FUSTO DI ACCIAIO          '.                  
002000   03   PIC X(29) VALUE 'NL/STALEN VAT                '.                  
002010   03   PIC X(29) VALUE 'NO/STÅLFAT                   '.                  
002100   03   PIC X(29) VALUE 'PL/BEBEN STALOWY             '.                  
002200   03   PIC X(29) VALUE 'SE/STÅLFAT                   '.                  
002300                                                                          
002400 01  FILLER REDEFINES FEMB-STEELDRUM-1.                                   
002500   03  FEMB-SRAD1  OCCURS 10 ASCENDING KEY IS IDSPRAK-S1                  
002600                             INDEXED BY S1-IX.                            
002700     05  IDSPRAK-S1          PIC X(2).                                    
002800     05  FILLER              PIC X(1).                                    
002900     05  TEEMB-S1            PIC X(26).                                   
003000                                                                          
003100 01  FEMB-STEELDRUM-2.                                                    
003200                                                                          
003300   03   PIC X(29) VALUE 'DE/FÄSSER, STAHL             '.                  
003400   03   PIC X(29) VALUE 'ES/BIDóNES DE ACERO          '.                  
003500   03   PIC X(29) VALUE 'FI/TERÄSLIERIÖT              '.                  
003600   03   PIC X(29) VALUE 'FR/FûTS, ACIER               '.                  
003700   03   PIC X(29) VALUE 'GB/STEEL  DRUMS              '.                  
003800   03   PIC X(29) VALUE 'IT/FURSTI DI ACCIAIO         '.                  
003900   03   PIC X(29) VALUE 'NL/STALEN VATEN              '.                  
003910   03   PIC X(29) VALUE 'NO/STÅLFAT                   '.                  
004000   03   PIC X(29) VALUE 'PL/BEBNY STALOWY             '.                  
004100   03   PIC X(29) VALUE 'SE/STÅLFAT                   '.                  
004200                                                                          
004300 01  FILLER REDEFINES FEMB-STEELDRUM-2.                                   
004400   03  FEMB-SRAD2  OCCURS 10 ASCENDING KEY IS IDSPRAK-S2                  
004500                             INDEXED BY S2-IX.                            
004600     05  IDSPRAK-S2          PIC X(2).                                    
004700     05  FILLER              PIC X(1).                                    
004800     05  TEEMB-S2            PIC X(26).                                   
