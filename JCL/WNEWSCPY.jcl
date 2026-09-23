//WNEWSCPY JOB (640W0030200WNEWSCPY,W100),'RTN W960D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*            -- KOPIERA VD-NEWSTAB -> WNEWSTAB                                
//COPY1   EXEC PGM=IEFBR14    W001ISPF                                          
//*TSO.ISPTLIB  DD                                                              
//*        DD  DSN=SYS2.F1TS00.TABLES          -- VD-TABLES FÖRST               
//*        DD  DSN=F1TSV2.PROD.TABLES          -- VD-TABLES FÖRST               
//******** DD  DSN=F1TSV2.NEWS.TABLES          -- GAMMALT VD LIB                
//*        DD  DSN=W.ISPF.TABLES,DISP=SHR      -- SEDAN VÅR                     
//*        DD  DSN=SYS1.ISPF.SISPTENU,DISP=SHR                                  
//SYSTSIN  DD *                                                                 
 ISPSTART CMD(%WNEWSCPY W.ISPF)                                                 
//*                                                                             
//*            -- KOPIERA WNEWSTAB -> EGEN (VD-)NEWSTAB                         
//COPY2   EXEC PGM=IEFBR14        -- DUMMY 00-08-25 K.A.  IEBCOPY               
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUT3   DD  SPACE=(CYL,(1,1))                                                
//SYSUT4   DD  SPACE=(CYL,(1,1))                                                
//DDIN     DD  DSN=W.ISPF.TABLES,DISP=SHR                                       
//DDTEMP   DD  DSN=&&TEMPTAB,SPACE=(TRK,(5,5,5))                                
//DDUT     DD  DSN=W.ISPF.TABLES,DISP=SHR                                       
//SYSIN    DD  *                                                                
  COPY INDD=DDIN,OUTDD=DDTEMP                                                   
  SELECT MEMBER=((WNEWSTAB,V530NEWS))                                           
  COPY INDD=((DDTEMP,R)),OUTDD=DDUT                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WNEWSCPY                                         
