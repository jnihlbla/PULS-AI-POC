//W97001FI JOB (540W0000100W97001FI,W100),'RTN W970V1',                         
//             CLASS=N,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ NJEVD                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//XMIT    EXEC VXFER                                                            
//SYSIN DD DATA,DLM='??'                                                        
 COPY DSN(VQ901.ACFUSERS(0))                                                    
      TODSN(W970.W970B1.W97001(+1))                                             
      DEST(NJEV1)                                                               
      MGMTCLAS(NOBACKUP)                                                        
      XFERID(W97001)                                                            
      RNOTIFER(SUB,SYSIN(ERROR),,                                               
               ULOG,F1XFV1.PROD.ERRLOG(+0),ALL)                                 
      RNOTIFOK(SUB,SYSIN(OK),,                                                  
               ULOG,F1XFV1.PROD.TOTLOG(+0),TOT).                                
++SYSIN (ERROR).                                                                
//W97001ER JOB (540W0000100W97001ER,W100),'RTN W970V1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ NJEVCP                                                              
/*ROUTE PRINT NJOV1                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> ABNORMAL    <== FILEMON-TRANSFER.                         *          
//*    (I.E SNOTIFER RECEIVED FROM FILEMON TRANSFER)                 *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN TILL V1:AN I W970V1 HAR EJ GÅTT BRA.      *          
//*   SE I ERROR-LOGGEN (F1XFVD.PROD.ERRLOG I V2:AN ELLER            *          
//*   F1XFV1.PROD.ERRLOG I V2:AN) EFTER ORSAKEN.                     *          
//*   XFERID: W97002                                                 *          
//*                                                                  *          
//********************************************************************          
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W97001FI                                         
++END.                                                                          
++SYSIN (OK).                                                                   
//W97001OK JOB (540W0000100W97001OK,W100),'RTN W970V1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ NJEVCP                                                              
/*ROUTE PRINT NJOV1                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W97001FI                                         
++END.                                                                          
??                                                                              
