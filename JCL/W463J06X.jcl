//W463J06X JOB (640W4630100W463J06X,W100),'RTN W463D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W463.W463D6.W4636A(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636B(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636C(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636D(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636E(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636F(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636G(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636H(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636I(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636J(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636K(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636L(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636M(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636N(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636O(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636P(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636Q(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636R(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636S(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636T(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636U(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W4636V(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PA(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PB(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PC(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PD(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PE(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PF(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PG(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PH(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PI(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PJ(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PK(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PL(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PM(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PN(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PO(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PP(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PQ(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PR(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PS(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PT(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PU(+0),DISP=SHR                              
//         DD  DSN=W463.W463D6.W463PV(+0),DISP=SHR                              
//SYSUT2   DD  DSN=W463.W463D6.W4636X(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463D6.W4636X(+1)                              
//*                                                                             
//    IF (EMPTYT.T.RC > 0) THEN                                                 
//      EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W463.W463D6.W4636X(+1),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J06X                                         
