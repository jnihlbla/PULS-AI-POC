//W463J0E3 JOB (640W4630100W463J0E3,W100),'RTN W463E3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
/*CNTL W46383,EXC                                                               
//*                                                                             
//* -- TEST PÅ OM DET REDAN FINNS 255 GENERATIONER                              
//* -- KOLLA VARFÖR!                                                            
//*ADDED BELOW FIX FOR 255 GEN ABEND , WE WILL WAIT TILL ALL THE                
//*GENS ARE CONSUMED IN W463S8 AND ONCE DONE WE CAN RESTART                     
//MAXTST  EXEC WGENRTST,DSIN=WIN.W463X3SE.W46383,GEN=255                        
//MAXIF   IF (MAXTST.T.RC = 0 ) THEN                                            
//ABEND   EXEC PGM=ABEND,PARM='ANVÄNDAR-ABEND 17'                               
//MAXIF   ENDIF                                                                 
//* --MQ STEP                                                                   
//WQREC   EXEC WZ11P013                                                         
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.ORDER                                                  
¤DSOUT WIN.W463X3SE.W46383                                                      
¤RECFM VB                                                                       
¤LRECL 1009                                                                     
/*                                                                              
//*                                                                             
//GENTST  EXEC WGENRTST,DSIN=WIN.W463X3SE.W46383,GEN=1                          
//GENIF   IF (GENTST.T.RC = 0 ) THEN  -----------------------                   
//ORDER   EXEC WSOP                                                             
  ORDER W463S8                                                                  
//GENIF   ENDIF                       -----------------------                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J0E3                                         
