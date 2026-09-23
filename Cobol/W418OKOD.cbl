002032*COMPOPT VRTEREUS=YES                                                     
010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030000 PROGRAM-ID.     W418OKOD.                                                
040000*AUTHOR.         JAN-ERIK FRANTZEN.                                       
050000*DATE-WRITTEN.   94/06/15.                                                
060000                                                                          
071024*                                                                         
080000*    FUNKTION:                                                            
081000*        PROGRAMMET ÄR ETT SUBPROGRAM SOM KONTROLLERAR                    
082000*        ORSAKSKODER FRÅN KREDITERINGEN.                                  
083000*        SVAR GES MED ETT ANTAL FLAGGOR.                                  
084029*                                                                         
085029*                                                                         
086030*    E-TRACKER: 1658417 DATED 2006-03-08                                  
086132*                850114 DATED 2006-11-15                                  
087029*                                                                         
160000                                                                          
160100 ENVIRONMENT DIVISION.                                                    
160300                                                                          
170400 DATA DIVISION.                                                           
170500                                                                          
171400 WORKING-STORAGE SECTION.                                                 
171500                                                                          
171609*    -- CHECKED BY WY2000                                                 
171709     SKIP3                                                                
171800 77  FELTEXT-STR                 PIC X(75)   VALUE SPACE.                 
171900 77  IDPGM                       PIC X(08)   VALUE 'W418OKOD'.            
172000 77  JA                          PIC X       VALUE 'J'.                   
172100 77  NEJ                         PIC X       VALUE 'N'.                   
172200 77  INDX                        PIC  9(3)   VALUE ZERO.                  
172300 77  WS-KDANMORS                 PIC  9(3)   VALUE ZERO.                  
172400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3)   VALUE +16 COMP-3.            
172500                                                                          
173000     SKIP2                                                                
173100 01  DYNAMISKA-SUBPROGRAM.                                                
173200*                                                                         
173300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
173500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
173600     SKIP2                                                                
173922******************************************************************        
174022* TABELL MED ALLA REGLER FÖR KDANMORS I KREDITERINGEN            *        
174122*                                                                *        
174222* FILLER    PIC X(33) VALUE '000/NNNNNNNNNNNNNNNNNNNNNNNNNN'.    *        
174322*                                                                *        
174422* POS:   NAMN:                                                   *        
174522* =============                                                  *        
174622* I FÖRSTA POSITIONEN ORSAKSKOD SEDAN SLASH SOM AVSKILJARE       *        
174722* DÄREFTER DE OLIKA FLAGGORNA I FÖLJANDE ORDNING.                *        
174822*                                                                *        
174922* 1      FL-ANALYSNR                                             *        
175022* 2      FL-ANT-LEVANM                                           *        
175122* 3      FL-PRIS-ZERO       (MÅSTE HA 0 I PRIS)                  *        
175222* 4      FL-EMBLEV                                               *        
175322* 5      FL-FRAKT                                                *        
175422* 6      FL-GODK-KOD        (GODKÄND KOD)                        *        
175522* 7      FL-GODK-PRIS-ZERO  (GODKÄND MED 0 I PRIS)               *        
175622* 8      FL-IDFAKT          (SKALL FINNAS)                       *        
175722* 9      FL-IDKONTO         (KONTO SKALL FINNAS MED)             *        
175822* 10     FL-KVALITET-AVVIK  (GODK. MED KVALITETSAVVIKELSE)       *        
175922* 11     FL-RETILL          (GENERERAR RETURTILLSTÅND)           *        
176022* 12     FL-EJ-SKROT        (EJ GODK. IHOP MED SKROTFLAGGA)      *        
176122* 13     FL-TIFAKT          (FAKTNR SKALL VARA MED)              *        
176222* 14     FL-TIDSGRAENS      (KOLLA TIDSGRÄNS?)                   *        
176322* 15     FL-VAERDEGRAENS    (KOLLA VÄRDEGRÄNS?)                  *        
176422* 16     FL-KDFAKTYP-R      (GODK. BARA FAKTYP 'R')              *        
176522* 17     FL-KRENOT-DIREKT   (KREDITNOTA DIREKT UTAN RETUR)       *        
176622* 18     FL-KRENOT-EFTER-RT (KREDITNOTA EFTER RETUR)             *        
176722* 19     FL-BELASTA-ANS-AVD (SKALL BELASTA AVD SOM FELAT)        *        
176822* 20     FL-TF              (TILLÄGGSFAKTURA)                    *        
176922* 21     FL-VALFRITT-PRIS   (VALFRITT PRIS KAN SÄTTAS)           *        
177022* 22     FL-HAEMTA-PRIS-FAKTURA (PRIS HÄMTAS FRÅN FAKTURAN)      *        
177122* 23     FL-PRISTILLAEMPA   (PRIS SÄTTS VIA PRISMODULEN)         *        
177222* 24     FL-INVENT-SDC      (GODK. FÖR INVENTERING PÅ SDC)       *        
177322* 25     FL-SALDOBOK-RETUR  (RETURER SOM SKALL BOKA SALDON)      *        
177422* 26     FL-INTERNUPPACKNING(INTERNUPPACKNING             )      *        
177522* 27     FL-KOD-SOM-BAER-TK (SKALL HA TILLÄGGSKOSTNADER   )      *        
177622* 28     FL-KRENOT-DIREKT-SKR(SKROTAS PÅ PLATS            )      *        
177722* 29     FL-LEVERANTOER     (KOD SOM UTLÖSER KRAV MOT LEVERANTÖR)*        
177822*                                                                *        
177922******************************************************************        
178000                                                                          
178100 01  TABELL.                                                              
178222   03  FILLER PIC X(33) VALUE '000/NJNNNJJJNJNNJJJJJNJNNJNJNNJNJ'.        
178322   03  FILLER PIC X(33) VALUE '001/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
178422   03  FILLER PIC X(33) VALUE '002/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
178522   03  FILLER PIC X(33) VALUE '003/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
179022   03  FILLER PIC X(33) VALUE '004/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
180022   03  FILLER PIC X(33) VALUE '005/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
190022   03  FILLER PIC X(33) VALUE '006/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
200022   03  FILLER PIC X(33) VALUE '007/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
201022   03  FILLER PIC X(33) VALUE '008/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202022   03  FILLER PIC X(33) VALUE '009/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202122   03  FILLER PIC X(33) VALUE '010/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202225   03  FILLER PIC X(33) VALUE '011/NJNNNJJJNNNJNNJNNNJJNNJJNNNNJ'.        
202322   03  FILLER PIC X(33) VALUE '012/NJJNNJJJNJJNNNJNNJJNNNNJJNNNJ'.        
202422   03  FILLER PIC X(33) VALUE '013/NJNNNJJJNJNNNNJNJNJNNNNJNNNJJ'.        
202522   03  FILLER PIC X(33) VALUE '014/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202622   03  FILLER PIC X(33) VALUE '015/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202722   03  FILLER PIC X(33) VALUE '016/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202822   03  FILLER PIC X(33) VALUE '017/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
202922   03  FILLER PIC X(33) VALUE '018/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
203022   03  FILLER PIC X(33) VALUE '019/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
203122   03  FILLER PIC X(33) VALUE '020/NJNNNJJJNJNJJJNJJNJNNJNJNNJNJ'.        
203225   03  FILLER PIC X(33) VALUE '021/NJNNNJJNNJNJNNNNNNJJNNJJNNNNJ'.        
203322   03  FILLER PIC X(33) VALUE '022/NJJNNJJNNJJNNNNNNJJNNNNJJNNNJ'.        
203422   03  FILLER PIC X(33) VALUE '023/NJJNNJJNNJNNNNNNJNJNNNNJNNNJJ'.        
203522   03  FILLER PIC X(33) VALUE '024/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
203627   03  FILLER PIC X(33) VALUE '025/NJNNNJJJNJNJJJNJJNJNNJNJNNJNJ'.        
203727   03  FILLER PIC X(33) VALUE '026/NJNNNJJNNJNJNNNNNNJJNNJJNNNNJ'.        
203827   03  FILLER PIC X(33) VALUE '027/NJJNNJJNNJJNNNNNNJJNNNNJJNNNJ'.        
203928   03  FILLER PIC X(33) VALUE '028/NJJNNJJNNJNNNNNNJNJNNNNJNNNJJ'.        
204022   03  FILLER PIC X(33) VALUE '029/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
204122   03  FILLER PIC X(33) VALUE '030/NJNNNJNJNNNNJJJJJNNNJNNNNNJNN'.        
204226   03  FILLER PIC X(33) VALUE '031/NJNNNJNJNNNNJJJJNNNJJNNNNNNNN'.        
204422   03  FILLER PIC X(33) VALUE '032/NJNNNJNNNNJJJNNJNJJNNNNNNNNNN'.        
204522   03  FILLER PIC X(33) VALUE '033/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
204622   03  FILLER PIC X(33) VALUE '034/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
204722   03  FILLER PIC X(33) VALUE '035/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
204822   03  FILLER PIC X(33) VALUE '036/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
204922   03  FILLER PIC X(33) VALUE '037/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205022   03  FILLER PIC X(33) VALUE '038/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205122   03  FILLER PIC X(33) VALUE '039/NNNNNJJNNNNNNNNNNNNNNNNNNNNNN'.        
205222   03  FILLER PIC X(33) VALUE '040/NJNNNJNJNNNNJJJJJNJNJJNNNNNNJ'.        
205322   03  FILLER PIC X(33) VALUE '041/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205422   03  FILLER PIC X(33) VALUE '042/NJNJJJJJNJJNJJJJNJJNNJNNJNJNJ'.        
205522   03  FILLER PIC X(33) VALUE '043/NJNJJJJJNNNNJJJJJNJNNJNNNNJJJ'.        
205622   03  FILLER PIC X(33) VALUE '044/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205722   03  FILLER PIC X(33) VALUE '045/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205822   03  FILLER PIC X(33) VALUE '046/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
205922   03  FILLER PIC X(33) VALUE '047/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206022   03  FILLER PIC X(33) VALUE '048/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206122   03  FILLER PIC X(33) VALUE '049/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206222   03  FILLER PIC X(33) VALUE '050/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206322   03  FILLER PIC X(33) VALUE '051/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206422   03  FILLER PIC X(33) VALUE '052/JJNNNJJNNJJNNNNNNJNNJNJNJNJNN'.        
206522   03  FILLER PIC X(33) VALUE '053/JJNNNJJNNNNNNNNNJNNNJNJNNNJJN'.        
206622   03  FILLER PIC X(33) VALUE '054/JJNNNJJNNJJNNNNNNJNNJNJNJNJNN'.        
206722   03  FILLER PIC X(33) VALUE '055/JJNNNJJNNNNNNNNNJNNNJNJNNNJJN'.        
206822   03  FILLER PIC X(33) VALUE '056/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
206922   03  FILLER PIC X(33) VALUE '057/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
207022   03  FILLER PIC X(33) VALUE '058/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
207122   03  FILLER PIC X(33) VALUE '059/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
207222   03  FILLER PIC X(33) VALUE '060/NJNNNJJJNJNNJJJJJNNNNJNNNNJNN'.        
207322   03  FILLER PIC X(33) VALUE '061/NJNNNJJNNJNNJJJJNNNJNJNNNNNNN'.        
207422   03  FILLER PIC X(33) VALUE '062/NJNNNJJJNJJNJJJJNJNNNJNNJNJNN'.        
207522   03  FILLER PIC X(33) VALUE '063/NJNNNJJJNJNNJJJJJNNNNJNNNNJJN'.        
207822   03  FILLER PIC X(33) VALUE '064/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
207922   03  FILLER PIC X(33) VALUE '065/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208022   03  FILLER PIC X(33) VALUE '066/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208122   03  FILLER PIC X(33) VALUE '067/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208222   03  FILLER PIC X(33) VALUE '068/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208322   03  FILLER PIC X(33) VALUE '069/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208422   03  FILLER PIC X(33) VALUE '070/NJNNNJJJNNNJJJNNJNNNNJNNNNNNN'.        
208522   03  FILLER PIC X(33) VALUE '071/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
208622   03  FILLER PIC X(33) VALUE '072/NJNNNJJJNJJJJJNJNJNNNJNNJNNNN'.        
208722   03  FILLER PIC X(33) VALUE '073/NJNNNJJJNNNJJJNJJNNNNJNNNNNJN'.        
208833   03  FILLER PIC X(33) VALUE '074/NJNNNJJJNJJJJJNNNNNNNJNNJNNNN'.        
208922   03  FILLER PIC X(33) VALUE '075/NJNNNJJJNJJJJJNJNJNNNJNNJNJNN'.        
209022   03  FILLER PIC X(33) VALUE '076/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
209122   03  FILLER PIC X(33) VALUE '077/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
209222   03  FILLER PIC X(33) VALUE '078/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
209322   03  FILLER PIC X(33) VALUE '079/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
209423   03  FILLER PIC X(33) VALUE '080/NJNNNJJNNJNNNNNNJNJNNNJNNNJNJ'.        
209522   03  FILLER PIC X(33) VALUE '081/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
209623   03  FILLER PIC X(33) VALUE '082/NJNNJJJJNJJNJNNJNJJNNJNNJNJNJ'.        
209723   03  FILLER PIC X(33) VALUE '083/NJNNJJJJNJNNJNNJJNJNNJNNNNJJJ'.        
209822   03  FILLER PIC X(33) VALUE '084/NJNNNJJJNNNNJNNJNNJNNNNNNNNNN'.        
209922   03  FILLER PIC X(33) VALUE '085/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210022   03  FILLER PIC X(33) VALUE '086/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210122   03  FILLER PIC X(33) VALUE '087/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210222   03  FILLER PIC X(33) VALUE '088/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210322   03  FILLER PIC X(33) VALUE '089/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210422   03  FILLER PIC X(33) VALUE '090/NJNNNJJNNJNJNNNNJNNNNNJNNNJNN'.        
210522   03  FILLER PIC X(33) VALUE '091/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
210622   03  FILLER PIC X(33) VALUE '092/NJNNNJJNNJJJNNNNNJNNNNJNJNJNN'.        
210822   03  FILLER PIC X(33) VALUE '093/NJNNNJJNNNNNNNNNJNNNNNJNNNJJN'.        
210922   03  FILLER PIC X(33) VALUE '094/NJNNNJJNNJJJNNNNNJNNNNJNJNJNN'.        
211022   03  FILLER PIC X(33) VALUE '095/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
211122   03  FILLER PIC X(33) VALUE '096/NJNNNJNNNNNNNNNNJNNNJNNNNNNNN'.        
211222   03  FILLER PIC X(33) VALUE '097/NJNNNNNNNNNNNNNNNNNNNNNNJJJNN'.        
211322   03  FILLER PIC X(33) VALUE '098/NJNNNJJNNJJJNNNNNJNNNNJNJNNNN'.        
211422   03  FILLER PIC X(33) VALUE '099/NJNNNJJNNNJNNNNNNJNNJNNNJNNNN'.        
211522   03  FILLER PIC X(33) VALUE '100/NNNNNNNNNNNNNNNNNNNNNNNNNNNNN'.        
211600   SKIP2                                                                  
211700 01  FILLER REDEFINES TABELL.                                             
211800    03 KDANMORS-TABELL OCCURS 101.                                        
211900       05 TAB-KDANMORS           PIC X(3).                                
212000       05 FILLER                 PIC X(1).                                
212100       05 TABELL-FLAGGOR.                                                 
212200          07 FL-ANALYSNR         PIC X(1).                                
212300          07 FL-ANT-LEVANM       PIC X(1).                                
212400          07 FL-PRIS-ZERO        PIC X(1).                                
212500          07 FL-EMBLEV           PIC X(1).                                
212600          07 FL-FRAKT            PIC X(1).                                
212700          07 FL-GODK-KOD         PIC X(1).                                
212800          07 FL-GODK-PRIS-ZERO   PIC X(1).                                
212900          07 FL-IDFAKT           PIC X(1).                                
213000          07 FL-IDKONTO          PIC X(1).                                
213100          07 FL-KVALITET-AVVIK   PIC X(1).                                
213200          07 FL-RETILL           PIC X(1).                                
213300          07 FL-EJ-SKROT         PIC X(1).                                
213400          07 FL-TIFAKT           PIC X(1).                                
213500          07 FL-TIDSGRAENS       PIC X(1).                                
213600          07 FL-VAERDEGRAENS     PIC X(1).                                
213700          07 FL-KDFAKTYP-R       PIC X(1).                                
213800          07 FL-KRENOT-DIREKT    PIC X(1).                                
213900          07 FL-KRENOT-EFTER-RT  PIC X(1).                                
214000          07 FL-BELASTA-ANS-AVD  PIC X(1).                                
214100          07 FL-TF               PIC X(1).                                
214200          07 FL-VALFRITT-PRIS    PIC X(1).                                
214300          07 FL-HEAMTA-PRIS-FAKTURA                                       
214400                                 PIC X(1).                                
214500          07 FL-PRISTILLAEMPA    PIC X(1).                                
214600          07 FL-INVENT-SDC       PIC X(1).                                
214700          07 FL-SALDOBOK-RETUR   PIC X(1).                                
214800          07 FL-INTERNUPPACKNING PIC X(1).                                
214900          07 FL-KOD-SOM-BAER-TK  PIC X(1).                                
215005          07 FL-KRENOT-DIREKT-SKR PIC X(1).                               
215122          07 FL-LEVERANTOER      PIC X(1).                                
215200     EJECT                                                                
215300*                                                               *         
215400 LINKAGE SECTION.                                                         
215500                                                                          
215600*   -COPY W418OKOD               -PRE LINK-.                              
215700                                                                          
215800     EJECT                                                                
215900                                                                          
216000 PROCEDURE DIVISION  USING LINK-W418OKOD.                                 
216100                                                                          
216200     MOVE LINK-KDANMORS       TO INDX                                     
216300     ADD 1                    TO INDX                                     
216400     MOVE TAB-KDANMORS (INDX) TO WS-KDANMORS                              
216500     IF WS-KDANMORS  = (INDX - 1)                                         
216600        MOVE TABELL-FLAGGOR (INDX)                                        
216700                              TO LINK-W418OKOD-GRP                        
216800     ELSE                                                                 
216900        MOVE 'ORSAKSKOD OCH TABELL-INDEX STÄMMER INTE ÖVERENS'            
217000                              TO FELTEXT-STR                              
217100                                                                          
217200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
217300     END-IF                                                               
217400                                                                          
217500     MOVE ZERO                TO RETURN-CODE                              
217600     GOBACK                                                               
218000     .                                                                    
220000     EJECT                                                                
