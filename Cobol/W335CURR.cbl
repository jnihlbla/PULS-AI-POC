000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W335CURR.                                                
000500 AUTHOR.         STINA MOGREN.                                            
000600 DATE-WRITTEN.   OKTOBER   -03.                                           
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - PROGRAMMET RÄKNAR OM VALUTA FÖR                                  
001310*        LOCAL-CURRENCY-MARKNADER.                                        
001400*        RÄKNAR OM SEK TILL LOKAL VALUTA ELLER                            
001500*        LOKAL VALUTA TILL SEK BEROENDE PÅ KDCALL.                        
001600*        KDCALL = 1 - LOKAL VALUTA TILL SEK                               
001700*        KDCALL = 2 - SEK TILL LOKAL VALUTA                               
001710*        KDCALL = 3 - LOCAL VALUTA TILL ANNAN VALUTA                      
001800*                                                                         
001900*        LÄNKAREA: W335CURR                                               
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W335CURR'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 01  BELOPP                 PIC S9(12)V9(6) VALUE ZERO COMP-3.            
003200                                                                          
003500*      --- VALID IDDC CODES                                               
003600*                                                                         
003610*01    -COPY WWDC99                                                       
003620       EJECT                                                              
003700     EJECT                                                                
003800 LINKAGE SECTION.                                                         
003900*                                                                         
004000*   -COPY W335CURR                                                        
004100*                                                                         
004200     EJECT                                                                
004400 PROCEDURE DIVISION  USING CURR-W335CURR.                                 
004500                                                                          
004600 STYR SECTION.                                                            
004700                                                                          
004800     PERFORM A-INIT                                                       
004900                                                                          
004910     IF CURR-PRKURS = 1.0 AND CURR-KDCALL < +3                            
004920       MOVE CURR-SUORDV-IN        TO CURR-SUORDV-UT                       
004921       MOVE CURR-PRARTSTD-IN      TO CURR-PRARTSTD-UT                     
004922       MOVE CURR-PRARTSJK-IN      TO CURR-PRARTSJK-UT                     
004923       MOVE CURR-PRARTVNA-IN      TO CURR-PRARTVNA-UT                     
004924       MOVE CURR-PRKURS           TO CURR-PRKURS-UT                       
004930     ELSE                                                                 
005000       IF CURR-KDCALL = +1                                                
005100                                                                          
005110***    RÄKNA OM FRÅN LOKAL VALUTA TILL SEK                                
005111         IF CURR-SUORDV-IN > +0                                           
005112          COMPUTE CURR-SUORDV-UT ROUNDED = CURR-SUORDV-IN *               
005120                                           CURR-PRKURS                    
005130         END-IF                                                           
005140         IF CURR-PRARTSTD-IN > +0                                         
005150          COMPUTE CURR-PRARTSTD-UT ROUNDED = CURR-PRARTSTD-IN *           
005160                                           CURR-PRKURS                    
005170         END-IF                                                           
005180         IF CURR-PRARTSJK-IN > +0                                         
005190          COMPUTE CURR-PRARTSJK-UT ROUNDED = CURR-PRARTSJK-IN *           
005191                                           CURR-PRKURS                    
005192         END-IF                                                           
005193         IF CURR-PRARTVNA-IN > +0                                         
005194          COMPUTE CURR-PRARTVNA-UT ROUNDED = CURR-PRARTVNA-IN *           
005195                                           CURR-PRKURS                    
005196         END-IF                                                           
005197         MOVE CURR-PRKURS   TO CURR-PRKURS-UT                             
005200       END-IF                                                             
005210       IF CURR-KDCALL = +2                                                
005220                                                                          
005230***      RÄKNA OM FRÅN SEK TILL LOKAL VALUTA                              
005231         IF CURR-SUORDV-IN > +0                                           
005232          COMPUTE CURR-SUORDV-UT ROUNDED = CURR-SUORDV-IN /               
005233               CURR-PRKURS                                                
005234         END-IF                                                           
005235         IF CURR-PRARTSTD-IN > +0                                         
005237          COMPUTE CURR-PRARTSTD-UT ROUNDED = CURR-PRARTSTD-IN /           
005238               CURR-PRKURS                                                
005239         END-IF                                                           
005240         IF CURR-PRARTSJK-IN > +0                                         
005241          COMPUTE CURR-PRARTSJK-UT ROUNDED = CURR-PRARTSJK-IN /           
005242               CURR-PRKURS                                                
005243         END-IF                                                           
005244         IF CURR-PRARTVNA-IN > +0                                         
005245          COMPUTE CURR-PRARTVNA-UT ROUNDED = CURR-PRARTVNA-IN /           
005246               CURR-PRKURS                                                
005247         END-IF                                                           
005248         MOVE CURR-PRKURS   TO CURR-PRKURS-UT                             
005249                                                                          
005250       END-IF                                                             
005251     END-IF                                                               
005260                                                                          
005280                                                                          
005281*    KURSEN MELLAN TVÅ FRÄMMANDE VALUTOR                                  
005500     IF CURR-KDCALL = +3                                                  
005600                                                                          
005610       IF CURR-PRKURS > +0 AND CURR-PRKURS-02 > +0                        
005620         COMPUTE CURR-PRKURS-UT ROUNDED = CURR-PRKURS /                   
005630                 CURR-PRKURS-02                                           
005640       END-IF                                                             
005700***      RÄKNA OM FRÅN LOKAL VALUTA TILL ANNAN VALUTA                     
005800       IF CURR-SUORDV-IN > +0                                             
005900          COMPUTE CURR-SUORDV-UT ROUNDED = CURR-SUORDV-IN *               
006000               CURR-PRKURS-UT                                             
006100       END-IF                                                             
006110       IF CURR-PRARTSTD-IN > +0                                           
006120          COMPUTE CURR-PRARTSTD-UT ROUNDED = CURR-PRARTSTD-IN *           
006130               CURR-PRKURS-UT                                             
006140       END-IF                                                             
006150       IF CURR-PRARTSJK-IN > +0                                           
006160          COMPUTE CURR-PRARTSJK-UT ROUNDED = CURR-PRARTSJK-IN *           
006170               CURR-PRKURS-UT                                             
006180       END-IF                                                             
006190       IF CURR-PRARTVNA-IN > +0                                           
006191          COMPUTE CURR-PRARTVNA-UT ROUNDED = CURR-PRARTVNA-IN *           
006192               CURR-PRKURS-UT                                             
006193       END-IF                                                             
006194                                                                          
006195     END-IF                                                               
006200                                                                          
006300     GOBACK                                                               
006400     .                                                                    
006500     EJECT                                                                
006600                                                                          
006700 A-INIT          SECTION.                                                 
006800                                                                          
007000     MOVE +0                   TO CURR-SUORDV-UT                          
007100                                  CURR-PRARTSTD-UT                        
007200                                  CURR-PRARTSJK-UT                        
007300                                  CURR-PRARTVNA-UT                        
007400                                  CURR-PRKURS-UT                          
007600     .                                                                    
007700     EJECT                                                                
007800                                                                          
