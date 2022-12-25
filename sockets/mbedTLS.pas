unit mbedTLS;

{$linklib ..\SubModules\mbedtls\library\libmbedtls.a}
{$linklib ..\SubModules\mbedtls\library\libmbedx509.a}
{$linklib ..\SubModules\mbedtls\library\libmbedcrypto.a}

{$linklib libmingwex.a}
{$linklib libgcc.a}
{$linklib libmsvcrt.a}
{$linklib libkernel32.a}
{$linklib libadvapi32.a}

interface

uses
  Classes,
  SysUtils;

type
  size_t = Integer;
  Psize_t = ^size_t;
  uint32_t = LongWord;

  Pmbedtls_ssl_session = ^mbedtls_shared;
  Pmbedtls_ssl_context = ^mbedtls_shared;
  Pmbedtls_ssl_config = ^mbedtls_shared;
  Pmbedtls_x509_crt = ^mbedtls_shared;
  Pmbedtls_x509_crl = ^mbedtls_shared;
  Pmbedtls_entropy_context = ^mbedtls_shared;
  Pmbedtls_ctr_drbg_context = ^mbedtls_shared;

  TEntropyFunc = function(Data: Pointer; output: PChar; len: size_t): Integer; cdecl;
  TrngFunc = function(Data: Pointer; output: PChar; len: size_t): Integer; cdecl;
  TdbgFunc = procedure(Data: Pointer; i: Integer; c: PChar; i2: Integer; c2: PChar); cdecl;
  TNetSendFunc = function(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl;
  TNetRecvFunc = function(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl;
  TNetRecvTimeoutFunc = function(ctx: Pointer; buf: Pointer; len: size_t; timeout: uint32_t): Integer; cdecl;
  TGetTimerFunc = function(ctx: Pointer): Integer; cdecl;
  TSetTimerFunc = procedure(ctx: Pointer; int_ms: uint32_t; fin_ms: uint32_t); cdecl;

  {$PACKRECORDS C}

  // Use a large shared record since record size can differ between mbedTLS versions/configurations
  mbedtls_shared = record
    Data: array[0..1023] of Byte;
  end;

  {
  mbedtls_x509_crt = record // size 308
    stuffing: array [0 .. 343] of byte;
  end;

  mbedtls_x509_crl = record // size 244
    stuffing: array [0 .. 243] of byte;
  end;

  mbedtls_ctr_drbg_context = record // size 320
    stuffing: array [0 .. 319] of byte;
  end;

  mbedtls_entropy_context = record // size 632
    stuffing: array [0 .. 631] of byte;
  end;

  mbedtls_ssl_session = record  // size 128
    stuffing: array [0 .. 127] of byte;
  end;

  mbedtls_ssl_config = record // size 208
    stuffing: array [0 .. 199] of byte;
  end;

  mbedtls_ssl_context = record // size 264
    stuffing: array [0 .. 303] of byte;
  end;
  }

const
  MBEDTLS_ERR_MPI_FILE_IO_ERROR = $0002;  // An error occurred while reading from or writing to a file.
  MBEDTLS_ERR_MPI_BAD_INPUT_DATA = $0004;  // Bad input parameters to function.
  MBEDTLS_ERR_MPI_INVALID_CHARACTER = $0006;  // There is an invalid character in the digit string.
  MBEDTLS_ERR_MPI_BUFFER_TOO_SMALL = $0008;  // The buffer is too small to write to.
  MBEDTLS_ERR_MPI_NEGATIVE_VALUE = $000A;  // The input arguments are negative or result in illegal output.
  MBEDTLS_ERR_MPI_DIVISION_BY_ZERO = $000C;  // The input argument for division is zero, which is not allowed.
  MBEDTLS_ERR_MPI_NOT_ACCEPTABLE = $000E;  // The input arguments are not acceptable.
  MBEDTLS_ERR_MPI_ALLOC_FAILED = $0010;  // Memory allocation failed.

  MBEDTLS_ERR_HMAC_DRBG_REQUEST_TOO_BIG = $0003;  // Too many random requested in single call.
  MBEDTLS_ERR_HMAC_DRBG_INPUT_TOO_BIG = $0005;  // Input too large (Entropy + additional).
  MBEDTLS_ERR_HMAC_DRBG_FILE_IO_ERROR = $0007;  // Read/write error in file.
  MBEDTLS_ERR_HMAC_DRBG_ENTROPY_SOURCE_FAILED = $0009;  // The entropy source failed.

  MBEDTLS_ERR_CCM_BAD_INPUT = $000D; // Bad input parameters to the function.
  MBEDTLS_ERR_CCM_AUTH_FAILED = $000F; // Authenticated decryption failed.
  MBEDTLS_ERR_CCM_HW_ACCEL_FAILED = $0011; // CCM hardware accelerator failed.

  MBEDTLS_ERR_GCM_AUTH_FAILED = $0012;  // Authenticated decryption failed.
  MBEDTLS_ERR_GCM_HW_ACCEL_FAILED = $0013;  // GCM hardware accelerator failed.
  MBEDTLS_ERR_GCM_BAD_INPUT = $0014;  // Bad input parameters to function.

  MBEDTLS_ERR_BLOWFISH_INVALID_KEY_LENGTH = $0016;  // Invalid key length.
  MBEDTLS_ERR_BLOWFISH_HW_ACCEL_FAILED = $0017;  // Blowfish hardware accelerator failed.
  MBEDTLS_ERR_BLOWFISH_INVALID_INPUT_LENGTH = $0018;  // Invalid data input length.

  MBEDTLS_ERR_ARC4_HW_ACCEL_FAILED = $0019;  // ARC4 hardware accelerator failed.

  MBEDTLS_ERR_THREADING_FEATURE_UNAVAILABLE = $001A;  // The selected feature is not available.
  MBEDTLS_ERR_THREADING_BAD_INPUT_DATA = $001C;  // Bad input parameters to function.
  MBEDTLS_ERR_THREADING_MUTEX_ERROR = $001E;  // Locking / unlocking / free failed with error code.

  MBEDTLS_ERR_AES_INVALID_KEY_LENGTH = $0020;  // Invalid key length.
  MBEDTLS_ERR_AES_INVALID_INPUT_LENGTH = $0022;  // Invalid data input length.

  MBEDTLS_ERR_AES_FEATURE_UNAVAILABLE = $0023;  // Feature not available. For example, an unsupported AES key size.
  MBEDTLS_ERR_AES_HW_ACCEL_FAILED = $0025;  // AES hardware accelerator failed.

  MBEDTLS_ERR_CAMELLIA_INVALID_KEY_LENGTH = $0024;  // Invalid key length.
  MBEDTLS_ERR_CAMELLIA_INVALID_INPUT_LENGTH = $0026;  // Invalid data input length.
  MBEDTLS_ERR_CAMELLIA_HW_ACCEL_FAILED = $0027;  // Camellia hardware accelerator failed.

  MBEDTLS_ERR_XTEA_INVALID_INPUT_LENGTH = $0028;  // The data input has an invalid length.
  MBEDTLS_ERR_XTEA_HW_ACCEL_FAILED = $0029;  // XTEA hardware accelerator failed.

  MBEDTLS_ERR_BASE64_BUFFER_TOO_SMALL = $002A;  // Output buffer too small.
  MBEDTLS_ERR_BASE64_INVALID_CHARACTER = $002C;  // Invalid character in input.

  MBEDTLS_ERR_MD2_HW_ACCEL_FAILED = $002B;  // MD2 hardware accelerator failed
  MBEDTLS_ERR_MD4_HW_ACCEL_FAILED = $002D;  // MD4 hardware accelerator failed
  MBEDTLS_ERR_MD5_HW_ACCEL_FAILED = $002F;  // MD5 hardware accelerator failed

  MBEDTLS_ERR_OID_NOT_FOUND = $002E;  // OID is not found.
  MBEDTLS_ERR_OID_BUF_TOO_SMALL = $000B;  // output buffer is too small

  MBEDTLS_ERR_PADLOCK_DATA_MISALIGNED = $0030;  // Input data should be aligned.

  MBEDTLS_ERR_RIPEMD160_HW_ACCEL_FAILED = $0031;  // RIPEMD160 hardware accelerator failed

  MBEDTLS_ERR_DES_INVALID_INPUT_LENGTH = $0032;  // The data input has an invalid length.
  MBEDTLS_ERR_DES_HW_ACCEL_FAILED = $0033;  // DES hardware accelerator failed.

  MBEDTLS_ERR_CTR_DRBG_ENTROPY_SOURCE_FAILED = $0034;  // The entropy source failed.
  MBEDTLS_ERR_CTR_DRBG_REQUEST_TOO_BIG = $0036;  // The requested random buffer length is too big.
  MBEDTLS_ERR_CTR_DRBG_INPUT_TOO_BIG = $0038;  // The input (entropy + additional data) is too large.
  MBEDTLS_ERR_CTR_DRBG_FILE_IO_ERROR = $003A;  // Read or write error in file.

  MBEDTLS_ERR_SHA1_HW_ACCEL_FAILED = $0035;  // SHA-1 hardware accelerator failed
  MBEDTLS_ERR_SHA256_HW_ACCEL_FAILED = $0037;  // SHA-256 hardware accelerator failed
  MBEDTLS_ERR_SHA512_HW_ACCEL_FAILED = $0039;  // SHA-512 hardware accelerator failed

  MBEDTLS_ERR_ENTROPY_SOURCE_FAILED = $003C;  // Critical entropy source failure.
  MBEDTLS_ERR_ENTROPY_MAX_SOURCES = $003E;  // No more sources can be added.
  MBEDTLS_ERR_ENTROPY_NO_SOURCES_DEFINED = $0040;  // No sources have been added to poll.
  MBEDTLS_ERR_ENTROPY_NO_STRONG_SOURCE = $003D;  // No strong sources have been added to poll.
  MBEDTLS_ERR_ENTROPY_FILE_IO_ERROR = $003F;  // Read/write error in file.

  MBEDTLS_ERR_NET_SOCKET_FAILED = $0042;  // Failed to open a socket.
  MBEDTLS_ERR_NET_CONNECT_FAILED = $0044;  // The connection to the given server / port failed.
  MBEDTLS_ERR_NET_BIND_FAILED = $0046;  // Binding of the socket failed.
  MBEDTLS_ERR_NET_LISTEN_FAILED = $0048;  // Could not listen on the socket.
  MBEDTLS_ERR_NET_ACCEPT_FAILED = $004A;  // Could not accept the incoming connection.
  MBEDTLS_ERR_NET_RECV_FAILED = $004C;  // Reading information from the socket failed.
  MBEDTLS_ERR_NET_SEND_FAILED = $004E;  // Sending information through the socket failed.
  MBEDTLS_ERR_NET_CONN_RESET = $0050;  // Connection was reset by peer.
  MBEDTLS_ERR_NET_UNKNOWN_HOST = $0052;  // Failed to get an IP address for the given hostname.
  MBEDTLS_ERR_NET_BUFFER_TOO_SMALL = $0043;  // Buffer is too small to hold the data.
  MBEDTLS_ERR_NET_INVALID_CONTEXT = $0045;  // The context is invalid, eg because it was free()ed.

  MBEDTLS_ERR_ASN1_OUT_OF_DATA = $0060;  // Out of data when parsing an ASN1 data structure.
  MBEDTLS_ERR_ASN1_UNEXPECTED_TAG = $0062;  // ASN1 tag was of an unexpected value.
  MBEDTLS_ERR_ASN1_INVALID_LENGTH = $0064;  // Error when trying to determine the length or invalid length.
  MBEDTLS_ERR_ASN1_LENGTH_MISMATCH = $0066;  // Actual length differs from expected length.
  MBEDTLS_ERR_ASN1_INVALID_DATA = $0068;  // Data is invalid. (not used)
  MBEDTLS_ERR_ASN1_ALLOC_FAILED = $006A;  // Memory allocation failed
  MBEDTLS_ERR_ASN1_BUF_TOO_SMALL = $006C;  // Buffer too small when writing ASN.1 data structure.

  MBEDTLS_ERR_CMAC_HW_ACCEL_FAILED = $007A;  // CMAC hardware accelerator failed.

  MBEDTLS_ERR_PEM_NO_HEADER_FOOTER_PRESENT = $1080;  // No PEM header or footer found.
  MBEDTLS_ERR_PEM_INVALID_DATA = $1100;  // PEM string is not as expected.
  MBEDTLS_ERR_PEM_ALLOC_FAILED = $1180;  // Failed to allocate memory.
  MBEDTLS_ERR_PEM_INVALID_ENC_IV = $1200;  // RSA IV is not in hex-format.
  MBEDTLS_ERR_PEM_UNKNOWN_ENC_ALG = $1280;  // Unsupported key encryption algorithm.
  MBEDTLS_ERR_PEM_PASSWORD_REQUIRED = $1300;  // Private key password can't be empty.
  MBEDTLS_ERR_PEM_PASSWORD_MISMATCH = $1380;  // Given private key password does not allow for correct decryption.
  MBEDTLS_ERR_PEM_FEATURE_UNAVAILABLE = $1400;  // Unavailable feature, e.g. hashing/encryption combination.
  MBEDTLS_ERR_PEM_BAD_INPUT_DATA = $1480;  // Bad input parameters to function.

  MBEDTLS_ERR_PKCS12_BAD_INPUT_DATA = $1F80;  // Bad input parameters to function.
  MBEDTLS_ERR_PKCS12_FEATURE_UNAVAILABLE = $1F00;  // Feature not available, e.g. unsupported encryption scheme.
  MBEDTLS_ERR_PKCS12_PBE_INVALID_FORMAT = $1E80;  // PBE ASN.1 data not as expected.
  MBEDTLS_ERR_PKCS12_PASSWORD_MISMATCH = $1E00;  // Given private key password does not allow for correct decryption.

  MBEDTLS_ERR_X509_FEATURE_UNAVAILABLE = $2080;  // Unavailable feature, e.g. RSA hashing/encryption combination.
  MBEDTLS_ERR_X509_UNKNOWN_OID = $2100;  // Requested OID is unknown.
  MBEDTLS_ERR_X509_INVALID_FORMAT = $2180;  // The CRT/CRL/CSR format is invalid, e.g. different type expected.
  MBEDTLS_ERR_X509_INVALID_VERSION = $2200;  // The CRT/CRL/CSR version element is invalid.
  MBEDTLS_ERR_X509_INVALID_SERIAL = $2280;  // The serial tag or value is invalid.
  MBEDTLS_ERR_X509_INVALID_ALG = $2300;  // The algorithm tag or value is invalid.
  MBEDTLS_ERR_X509_INVALID_NAME = $2380;  // The name tag or value is invalid.
  MBEDTLS_ERR_X509_INVALID_DATE = $2400;  // The date tag or value is invalid.
  MBEDTLS_ERR_X509_INVALID_SIGNATURE = $2480;  // The signature tag or value invalid.
  MBEDTLS_ERR_X509_INVALID_EXTENSIONS = $2500;  // The extension tag or value is invalid.
  MBEDTLS_ERR_X509_UNKNOWN_VERSION = $2580;  // CRT/CRL/CSR has an unsupported version number.
  MBEDTLS_ERR_X509_UNKNOWN_SIG_ALG = $2600;  // Signature algorithm (oid) is unsupported.
  MBEDTLS_ERR_X509_SIG_MISMATCH = $2680;  // Signature algorithms do not match. (see \c ::mbedtls_x509_crt sig_oid)
  MBEDTLS_ERR_X509_CERT_VERIFY_FAILED = $2700;  // Certificate verification failed, e.g. CRL, CA or signature check failed.
  MBEDTLS_ERR_X509_CERT_UNKNOWN_FORMAT = $2780;  // Format not recognized as DER or PEM.
  MBEDTLS_ERR_X509_BAD_INPUT_DATA = $2800;  // Input invalid.
  MBEDTLS_ERR_X509_ALLOC_FAILED = $2880;  // Allocation of memory failed.
  MBEDTLS_ERR_X509_FILE_IO_ERROR = $2900;  // Read/write of file failed.
  MBEDTLS_ERR_X509_BUFFER_TOO_SMALL = $2980;  // Destination buffer is too small.
  MBEDTLS_ERR_X509_FATAL_ERROR = $3000;  // A fatal error occured, eg the chain is too long or the vrfy callback failed.

  MBEDTLS_ERR_PKCS5_BAD_INPUT_DATA = $2F80;  // Bad input parameters to function.
  MBEDTLS_ERR_PKCS5_INVALID_FORMAT = $2F00;  // Unexpected ASN.1 data.
  MBEDTLS_ERR_PKCS5_FEATURE_UNAVAILABLE = $2E80;  // Requested encryption or digest alg not available.
  MBEDTLS_ERR_PKCS5_PASSWORD_MISMATCH = $2E00;  // Given private key password does not allow for correct decryption.

  MBEDTLS_ERR_DHM_BAD_INPUT_DATA = $3080;  // Bad input parameters.
  MBEDTLS_ERR_DHM_READ_PARAMS_FAILED = $3100;  // Reading of the DHM parameters failed.
  MBEDTLS_ERR_DHM_MAKE_PARAMS_FAILED = $3180;  // Making of the DHM parameters failed.
  MBEDTLS_ERR_DHM_READ_PUBLIC_FAILED = $3200;  // Reading of the public values failed.
  MBEDTLS_ERR_DHM_MAKE_PUBLIC_FAILED = $3280;  // Making of the public value failed.
  MBEDTLS_ERR_DHM_CALC_SECRET_FAILED = $3300;  // Calculation of the DHM secret failed.
  MBEDTLS_ERR_DHM_INVALID_FORMAT = $3380;  // The ASN.1 data is not formatted correctly.
  MBEDTLS_ERR_DHM_ALLOC_FAILED = $3400;  // Allocation of memory failed.
  MBEDTLS_ERR_DHM_FILE_IO_ERROR = $3480;  // Read or write of file failed.
  MBEDTLS_ERR_DHM_HW_ACCEL_FAILED = $3500;  // DHM hardware accelerator failed.
  MBEDTLS_ERR_DHM_SET_GROUP_FAILED = $3580;  // Setting the modulus and generator failed.

  MBEDTLS_ERR_PK_ALLOC_FAILED = $3F80;  // Memory allocation failed.
  MBEDTLS_ERR_PK_TYPE_MISMATCH = $3F00;  // Type mismatch, eg attempt to encrypt with an ECDSA key
  MBEDTLS_ERR_PK_BAD_INPUT_DATA = $3E80;  // Bad input parameters to function.
  MBEDTLS_ERR_PK_FILE_IO_ERROR = $3E00;  // Read/write of file failed.
  MBEDTLS_ERR_PK_KEY_INVALID_VERSION = $3D80;  // Unsupported key version
  MBEDTLS_ERR_PK_KEY_INVALID_FORMAT = $3D00;  // Invalid key tag or value.
  MBEDTLS_ERR_PK_UNKNOWN_PK_ALG = $3C80;  // Key algorithm is unsupported (only RSA and EC are supported).
  MBEDTLS_ERR_PK_PASSWORD_REQUIRED = $3C00;  // Private key password can't be empty.
  MBEDTLS_ERR_PK_PASSWORD_MISMATCH = $3B80;  // Given private key password does not allow for correct decryption.
  MBEDTLS_ERR_PK_INVALID_PUBKEY = $3B00;  // The pubkey tag or value is invalid (only RSA and EC are supported).
  MBEDTLS_ERR_PK_INVALID_ALG = $3A80;  // The algorithm tag or value is invalid.
  MBEDTLS_ERR_PK_UNKNOWN_NAMED_CURVE = $3A00;  // Elliptic curve is unsupported (only NIST curves are supported).
  MBEDTLS_ERR_PK_FEATURE_UNAVAILABLE = $3980;  // Unavailable feature, e.g. RSA disabled for RSA key.
  MBEDTLS_ERR_PK_SIG_LEN_MISMATCH = $3900;  // The signature is valid but its length is less than expected.
  MBEDTLS_ERR_PK_HW_ACCEL_FAILED = $3880;  // PK hardware accelerator failed.

  MBEDTLS_ERR_RSA_BAD_INPUT_DATA = $4080;  // Bad input parameters to function.
  MBEDTLS_ERR_RSA_INVALID_PADDING = $4100;  // Input data contains invalid padding and is rejected.
  MBEDTLS_ERR_RSA_KEY_GEN_FAILED = $4180;  // Something failed during generation of a key.
  MBEDTLS_ERR_RSA_KEY_CHECK_FAILED = $4200;  // Key failed to pass the validity check of the library.
  MBEDTLS_ERR_RSA_PUBLIC_FAILED = $4280;  // The public key operation failed.
  MBEDTLS_ERR_RSA_PRIVATE_FAILED = $4300;  // The private key operation failed.
  MBEDTLS_ERR_RSA_VERIFY_FAILED = $4380;  // The PKCS#1 verification failed.
  MBEDTLS_ERR_RSA_OUTPUT_TOO_LARGE = $4400;  // The output buffer for decryption is not large enough.
  MBEDTLS_ERR_RSA_RNG_FAILED = $4480;  // The random generator failed to generate non-zeros.
  MBEDTLS_ERR_RSA_UNSUPPORTED_OPERATION = $4500;  // The implementation does not offer the requested operation, for example, because of security violations or lack of functionality.
  MBEDTLS_ERR_RSA_HW_ACCEL_FAILED = $4580;  // RSA hardware accelerator failed.

  MBEDTLS_ERR_ECP_BAD_INPUT_DATA = $4F80;  // Bad input parameters to function.
  MBEDTLS_ERR_ECP_BUFFER_TOO_SMALL = $4F00;  // The buffer is too small to write to.
  MBEDTLS_ERR_ECP_FEATURE_UNAVAILABLE = $4E80;  // Requested curve not available.
  MBEDTLS_ERR_ECP_VERIFY_FAILED = $4E00;  // The signature is not valid.
  MBEDTLS_ERR_ECP_ALLOC_FAILED = $4D80;  // Memory allocation failed.
  MBEDTLS_ERR_ECP_RANDOM_FAILED = $4D00;  // Generation of random value, such as (ephemeral) key, failed.
  MBEDTLS_ERR_ECP_INVALID_KEY = $4C80;  // Invalid private or public key.
  MBEDTLS_ERR_ECP_SIG_LEN_MISMATCH = $4C00;  // Signature is valid but shorter than the user-supplied length.
  MBEDTLS_ERR_ECP_HW_ACCEL_FAILED = $4B80;  // ECP hardware accelerator failed.

  MBEDTLS_ERR_MD_FEATURE_UNAVAILABLE = $5080;  // The selected feature is not available.
  MBEDTLS_ERR_MD_BAD_INPUT_DATA = $5100;  // Bad input parameters to function.
  MBEDTLS_ERR_MD_ALLOC_FAILED = $5180;  // Failed to allocate memory.
  MBEDTLS_ERR_MD_FILE_IO_ERROR = $5200;  // Opening or reading of file failed.
  MBEDTLS_ERR_MD_HW_ACCEL_FAILED = $5280;  // MD hardware accelerator failed.

  MBEDTLS_ERR_CIPHER_FEATURE_UNAVAILABLE = $6080;  // The selected feature is not available.
  MBEDTLS_ERR_CIPHER_BAD_INPUT_DATA = $6100;  // Bad input parameters.
  MBEDTLS_ERR_CIPHER_ALLOC_FAILED = $6180;  // Failed to allocate memory.
  MBEDTLS_ERR_CIPHER_INVALID_PADDING = $6200;  // Input data contains invalid padding and is rejected.
  MBEDTLS_ERR_CIPHER_FULL_BLOCK_EXPECTED = $6280;  // Decryption of block requires a full block.
  MBEDTLS_ERR_CIPHER_AUTH_FAILED = $6300;  // Authentication failed (for AEAD modes).
  MBEDTLS_ERR_CIPHER_INVALID_CONTEXT = $6380;  // The context is invalid. For example, because it was freed.
  MBEDTLS_ERR_CIPHER_HW_ACCEL_FAILED = $6400;  // Cipher hardware accelerator failed.

  MBEDTLS_ERR_SSL_FEATURE_UNAVAILABLE = $7080;  // The requested feature is not available.
  MBEDTLS_ERR_SSL_BAD_INPUT_DATA = $7100;  // Bad input parameters to function.
  MBEDTLS_ERR_SSL_INVALID_MAC = $7180;  // Verification of the message MAC failed.
  MBEDTLS_ERR_SSL_INVALID_RECORD = $7200;  // An invalid SSL record was received.
  MBEDTLS_ERR_SSL_CONN_EOF = $7280;  // The connection indicated an EOF.
  MBEDTLS_ERR_SSL_UNKNOWN_CIPHER = $7300;  // An unknown cipher was received.
  MBEDTLS_ERR_SSL_NO_CIPHER_CHOSEN = $7380;  // The server has no ciphersuites in common with the client.
  MBEDTLS_ERR_SSL_NO_RNG = $7400;  // No RNG was provided to the SSL module.
  MBEDTLS_ERR_SSL_NO_CLIENT_CERTIFICATE = $7480;  // No client certification received from the client, but required by the authentication mode.
  MBEDTLS_ERR_SSL_CERTIFICATE_TOO_LARGE = $7500;  // Our own certificate(s) is/are too large to send in an SSL message.
  MBEDTLS_ERR_SSL_CERTIFICATE_REQUIRED = $7580;  // The own certificate is not set, but needed by the server.
  MBEDTLS_ERR_SSL_PRIVATE_KEY_REQUIRED = $7600;  // The own private key or pre-shared key is not set, but needed.
  MBEDTLS_ERR_SSL_CA_CHAIN_REQUIRED = $7680;  // No CA Chain is set, but required to operate.
  MBEDTLS_ERR_SSL_UNEXPECTED_MESSAGE = $7700;  // An unexpected message was received from our peer.
  MBEDTLS_ERR_SSL_FATAL_ALERT_MESSAGE = $7780;  // A fatal alert message was received from our peer.
  MBEDTLS_ERR_SSL_PEER_VERIFY_FAILED = $7800;  // Verification of our peer failed.
  MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY = $7880;  // The peer notified us that the connection is going to be closed.
  MBEDTLS_ERR_SSL_BAD_HS_CLIENT_HELLO = $7900;  // Processing of the ClientHello handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_SERVER_HELLO = $7980;  // Processing of the ServerHello handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_CERTIFICATE = $7A00;  // Processing of the Certificate handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_CERTIFICATE_REQUEST = $7A80;  // Processing of the CertificateRequest handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_SERVER_KEY_EXCHANGE = $7B00;  // Processing of the ServerKeyExchange handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_SERVER_HELLO_DONE = $7B80;  // Processing of the ServerHelloDone handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_CLIENT_KEY_EXCHANGE = $7C00;  // Processing of the ClientKeyExchange handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_CLIENT_KEY_EXCHANGE_RP = $7C80;  // Processing of the ClientKeyExchange handshake message failed in DHM / ECDH Read Public.
  MBEDTLS_ERR_SSL_BAD_HS_CLIENT_KEY_EXCHANGE_CS = $7D00;  // Processing of the ClientKeyExchange handshake message failed in DHM / ECDH Calculate Secret.
  MBEDTLS_ERR_SSL_BAD_HS_CERTIFICATE_VERIFY = $7D80;  // Processing of the CertificateVerify handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_CHANGE_CIPHER_SPEC = $7E00;  // Processing of the ChangeCipherSpec handshake message failed.
  MBEDTLS_ERR_SSL_BAD_HS_FINISHED = $7E80;  // Processing of the Finished handshake message failed.
  MBEDTLS_ERR_SSL_ALLOC_FAILED = $7F00;  // Memory allocation failed
  MBEDTLS_ERR_SSL_HW_ACCEL_FAILED = $7F80;  // Hardware acceleration function returned with error
  MBEDTLS_ERR_SSL_HW_ACCEL_FALLTHROUGH = $6F80;  // Hardware acceleration function skipped / left alone data
  MBEDTLS_ERR_SSL_COMPRESSION_FAILED = $6F00;  // Processing of the compression / decompression failed
  MBEDTLS_ERR_SSL_BAD_HS_PROTOCOL_VERSION = $6E80;  // Handshake protocol not within min/max boundaries
  MBEDTLS_ERR_SSL_BAD_HS_NEW_SESSION_TICKET = $6E00;  // Processing of the NewSessionTicket handshake message failed.
  MBEDTLS_ERR_SSL_SESSION_TICKET_EXPIRED = $6D80;  // Session ticket has expired.
  MBEDTLS_ERR_SSL_PK_TYPE_MISMATCH = $6D00;  // Public key type mismatch (eg, asked for RSA key exchange and presented EC key)
  MBEDTLS_ERR_SSL_UNKNOWN_IDENTITY = $6C80;  // Unknown identity received (eg, PSK identity)
  MBEDTLS_ERR_SSL_INTERNAL_ERROR = $6C00;  // Internal error (eg, unexpected failure in lower-level module)
  MBEDTLS_ERR_SSL_COUNTER_WRAPPING = $6B80;  // A counter would wrap (eg, too many messages exchanged).
  MBEDTLS_ERR_SSL_WAITING_SERVER_HELLO_RENEGO = $6B00;  // Unexpected message at ServerHello in renegotiation.
  MBEDTLS_ERR_SSL_HELLO_VERIFY_REQUIRED = $6A80;  // DTLS client must retry for hello verification
  MBEDTLS_ERR_SSL_BUFFER_TOO_SMALL = $6A00;  // A buffer is too small to receive or write a message
  MBEDTLS_ERR_SSL_NO_USABLE_CIPHERSUITE = $6980;  // None of the common ciphersuites is usable (eg, no suitable certificate, see debug messages).
  MBEDTLS_ERR_SSL_WANT_READ = $6900;  // Connection requires a read call.
  MBEDTLS_ERR_SSL_WANT_WRITE = $6880;  // Connection requires a write call.
  MBEDTLS_ERR_SSL_TIMEOUT = $6800;  // The operation timed out.
  MBEDTLS_ERR_SSL_CLIENT_RECONNECT = $6780;  // The client initiated a reconnect from the same port.
  MBEDTLS_ERR_SSL_UNEXPECTED_RECORD = $6700;  // Record header looks valid but is not expected.
  MBEDTLS_ERR_SSL_NON_FATAL = $6680;  // The alert message received indicates a non-fatal error.
  MBEDTLS_ERR_SSL_INVALID_VERIFY_HASH = $6600;  // Couldn't set the hash for verifying CertificateVerify
  MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS = $7000;

  MBEDTLS_SSL_MAJOR_VERSION_3 = 3;
  MBEDTLS_SSL_MINOR_VERSION_0 = 0;  // SSL v3.0
  MBEDTLS_SSL_MINOR_VERSION_1 = 1;  // TLS v1.0
  MBEDTLS_SSL_MINOR_VERSION_2 = 2;  // TLS v1.1
  MBEDTLS_SSL_MINOR_VERSION_3 = 3;  // TLS v1.2

  MBEDTLS_SSL_TRANSPORT_STREAM = 0;
  MBEDTLS_SSL_TRANSPORT_DATAGRAM = 1;

  MBEDTLS_SSL_MAX_HOST_NAME_LEN = 255;

  MBEDTLS_SSL_MAX_FRAG_LEN_NONE = 0;
  MBEDTLS_SSL_MAX_FRAG_LEN_512 = 1;
  MBEDTLS_SSL_MAX_FRAG_LEN_1024 = 2;
  MBEDTLS_SSL_MAX_FRAG_LEN_2048 = 3;
  MBEDTLS_SSL_MAX_FRAG_LEN_4096 = 4;
  MBEDTLS_SSL_MAX_FRAG_LEN_INVALID = 5;

  MBEDTLS_SSL_IS_CLIENT = 0;
  MBEDTLS_SSL_IS_SERVER = 1;

  MBEDTLS_SSL_IS_NOT_FALLBACK = 0;
  MBEDTLS_SSL_IS_FALLBACK = 1;

  MBEDTLS_SSL_EXTENDED_MS_DISABLED = 0;
  MBEDTLS_SSL_EXTENDED_MS_ENABLED = 1;

  MBEDTLS_SSL_ETM_DISABLED = 0;
  MBEDTLS_SSL_ETM_ENABLED = 1;

  MBEDTLS_SSL_COMPRESS_NULL = 0;
  MBEDTLS_SSL_COMPRESS_DEFLATE = 1;

  MBEDTLS_SSL_VERIFY_NONE = 0;
  MBEDTLS_SSL_VERIFY_OPTIONAL = 1;
  MBEDTLS_SSL_VERIFY_REQUIRED = 2;
  MBEDTLS_SSL_VERIFY_UNSET = 3;

  MBEDTLS_SSL_LEGACY_RENEGOTIATION = 0;
  MBEDTLS_SSL_SECURE_RENEGOTIATION = 1;

  MBEDTLS_SSL_RENEGOTIATION_DISABLED = 0;
  MBEDTLS_SSL_RENEGOTIATION_ENABLED = 1;

  MBEDTLS_SSL_ANTI_REPLAY_DISABLED = 0;
  MBEDTLS_SSL_ANTI_REPLAY_ENABLED = 1;

  MBEDTLS_SSL_RENEGOTIATION_NOT_ENFORCED = -1;
  MBEDTLS_SSL_RENEGO_MAX_RECORDS_DEFAULT = 16;

  MBEDTLS_SSL_LEGACY_NO_RENEGOTIATION = 0;
  MBEDTLS_SSL_LEGACY_ALLOW_RENEGOTIATION = 1;
  MBEDTLS_SSL_LEGACY_BREAK_HANDSHAKE = 2;

  MBEDTLS_SSL_TRUNC_HMAC_DISABLED = 0;
  MBEDTLS_SSL_TRUNC_HMAC_ENABLED = 1;
  MBEDTLS_SSL_TRUNCATED_HMAC_LEN = 10;

  MBEDTLS_SSL_SESSION_TICKETS_DISABLED = 0;
  MBEDTLS_SSL_SESSION_TICKETS_ENABLED = 1;

  MBEDTLS_SSL_CBC_RECORD_SPLITTING_DISABLED = 0;
  MBEDTLS_SSL_CBC_RECORD_SPLITTING_ENABLED = 1;

  MBEDTLS_SSL_ARC4_ENABLED = 0;
  MBEDTLS_SSL_ARC4_DISABLED = 1;

  MBEDTLS_SSL_PRESET_DEFAULT = 0;
  MBEDTLS_SSL_PRESET_SUITEB = 2;

  MBEDTLS_SSL_CERT_REQ_CA_LIST_ENABLED = 1;
  MBEDTLS_SSL_CERT_REQ_CA_LIST_DISABLED = 0;

  MBEDTLS_SSL_DTLS_TIMEOUT_DFL_MIN = 1000;
  MBEDTLS_SSL_DTLS_TIMEOUT_DFL_MAX = 60000;

  MBEDTLS_SSL_INITIAL_HANDSHAKE = 0;
  MBEDTLS_SSL_RENEGOTIATION_IN_PROGRESS = 1;
  MBEDTLS_SSL_RENEGOTIATION_DONE = 2;
  MBEDTLS_SSL_RENEGOTIATION_PENDING = 3;

  MBEDTLS_SSL_RETRANS_PREPARING = 0;
  MBEDTLS_SSL_RETRANS_SENDING = 1;
  MBEDTLS_SSL_RETRANS_WAITING = 2;
  MBEDTLS_SSL_RETRANS_FINISHED = 3;

  MBEDTLS_SSL_MSG_CHANGE_CIPHER_SPEC = 20;
  MBEDTLS_SSL_MSG_ALERT = 21;
  MBEDTLS_SSL_MSG_HANDSHAKE = 22;
  MBEDTLS_SSL_MSG_APPLICATION_DATA = 23;

  MBEDTLS_SSL_ALERT_LEVEL_WARNING = 1;
  MBEDTLS_SSL_ALERT_LEVEL_FATAL = 2;

  MBEDTLS_SSL_ALERT_MSG_CLOSE_NOTIFY = 0;
  MBEDTLS_SSL_ALERT_MSG_UNEXPECTED_MESSAGE = 10;
  MBEDTLS_SSL_ALERT_MSG_BAD_RECORD_MAC = 20;
  MBEDTLS_SSL_ALERT_MSG_DECRYPTION_FAILED = 21;
  MBEDTLS_SSL_ALERT_MSG_RECORD_OVERFLOW = 22;
  MBEDTLS_SSL_ALERT_MSG_DECOMPRESSION_FAILURE = 30;
  MBEDTLS_SSL_ALERT_MSG_HANDSHAKE_FAILURE = 40;
  MBEDTLS_SSL_ALERT_MSG_NO_CERT = 41;
  MBEDTLS_SSL_ALERT_MSG_BAD_CERT = 42;
  MBEDTLS_SSL_ALERT_MSG_UNSUPPORTED_CERT = 43;
  MBEDTLS_SSL_ALERT_MSG_CERT_REVOKED = 44;
  MBEDTLS_SSL_ALERT_MSG_CERT_EXPIRED = 45;
  MBEDTLS_SSL_ALERT_MSG_CERT_UNKNOWN = 46;
  MBEDTLS_SSL_ALERT_MSG_ILLEGAL_PARAMETER = 47;
  MBEDTLS_SSL_ALERT_MSG_UNKNOWN_CA = 48;
  MBEDTLS_SSL_ALERT_MSG_ACCESS_DENIED = 49;
  MBEDTLS_SSL_ALERT_MSG_DECODE_ERROR = 50;
  MBEDTLS_SSL_ALERT_MSG_DECRYPT_ERROR = 51;
  MBEDTLS_SSL_ALERT_MSG_EXPORT_RESTRICTION = 60;
  MBEDTLS_SSL_ALERT_MSG_PROTOCOL_VERSION = 70;
  MBEDTLS_SSL_ALERT_MSG_INSUFFICIENT_SECURITY = 71;
  MBEDTLS_SSL_ALERT_MSG_INTERNAL_ERROR = 80;
  MBEDTLS_SSL_ALERT_MSG_INAPROPRIATE_FALLBACK = 86;
  MBEDTLS_SSL_ALERT_MSG_USER_CANCELED = 90;
  MBEDTLS_SSL_ALERT_MSG_NO_RENEGOTIATION = 100;
  MBEDTLS_SSL_ALERT_MSG_UNSUPPORTED_EXT = 110;
  MBEDTLS_SSL_ALERT_MSG_UNRECOGNIZED_NAME = 112;
  MBEDTLS_SSL_ALERT_MSG_UNKNOWN_PSK_IDENTITY = 115;
  MBEDTLS_SSL_ALERT_MSG_NO_APPLICATION_PROTOCOL = 120;

  MBEDTLS_SSL_HS_HELLO_REQUEST = 0;
  MBEDTLS_SSL_HS_CLIENT_HELLO = 1;
  MBEDTLS_SSL_HS_SERVER_HELLO = 2;
  MBEDTLS_SSL_HS_HELLO_VERIFY_REQUEST = 3;
  MBEDTLS_SSL_HS_NEW_SESSION_TICKET = 4;
  MBEDTLS_SSL_HS_CERTIFICATE = 11;
  MBEDTLS_SSL_HS_SERVER_KEY_EXCHANGE = 12;
  MBEDTLS_SSL_HS_CERTIFICATE_REQUEST = 13;
  MBEDTLS_SSL_HS_SERVER_HELLO_DONE = 14;
  MBEDTLS_SSL_HS_CERTIFICATE_VERIFY = 15;
  MBEDTLS_SSL_HS_CLIENT_KEY_EXCHANGE = 16;
  MBEDTLS_SSL_HS_FINISHED = 20;

  MBEDTLS_TLS_EXT_SERVERNAME = 0;
  MBEDTLS_TLS_EXT_SERVERNAME_HOSTNAME = 0;
  MBEDTLS_TLS_EXT_MAX_FRAGMENT_LENGTH = 1;
  MBEDTLS_TLS_EXT_TRUNCATED_HMAC = 4;
  MBEDTLS_TLS_EXT_SUPPORTED_ELLIPTIC_CURVES = 10;
  MBEDTLS_TLS_EXT_SUPPORTED_POINT_FORMATS = 11;
  MBEDTLS_TLS_EXT_SIG_ALG = 13;
  MBEDTLS_TLS_EXT_ALPN = 16;
  MBEDTLS_TLS_EXT_ENCRYPT_THEN_MAC = 22;
  MBEDTLS_TLS_EXT_EXTENDED_MASTER_SECRET = $0017;
  MBEDTLS_TLS_EXT_SESSION_TICKET = 35;
  MBEDTLS_TLS_EXT_ECJPAKE_KKPP = 256;
  MBEDTLS_TLS_EXT_RENEGOTIATION_INFO = $FF01;

  MBEDTLS_SSL_HELLO_REQUEST = 0;
  MBEDTLS_SSL_CLIENT_HELLO = 1;
  MBEDTLS_SSL_SERVER_HELLO = 2;
  MBEDTLS_SSL_SERVER_CERTIFICATE = 3;
  MBEDTLS_SSL_SERVER_KEY_EXCHANGE = 4;
  MBEDTLS_SSL_CERTIFICATE_REQUEST = 5;
  MBEDTLS_SSL_SERVER_HELLO_DONE = 6;
  MBEDTLS_SSL_CLIENT_CERTIFICATE = 7;
  MBEDTLS_SSL_CLIENT_KEY_EXCHANGE = 8;
  MBEDTLS_SSL_CERTIFICATE_VERIFY = 9;
  MBEDTLS_SSL_CLIENT_CHANGE_CIPHER_SPEC = 10;
  MBEDTLS_SSL_CLIENT_FINISHED = 11;
  MBEDTLS_SSL_SERVER_CHANGE_CIPHER_SPEC = 12;
  MBEDTLS_SSL_SERVER_FINISHED = 13;
  MBEDTLS_SSL_FLUSH_BUFFERS = 14;
  MBEDTLS_SSL_HANDSHAKE_WRAPUP = 15;
  MBEDTLS_SSL_HANDSHAKE_OVER = 16;
  MBEDTLS_SSL_SERVER_NEW_SESSION_TICKET = 17;
  MBEDTLS_SSL_SERVER_HELLO_VERIFY_REQUEST_SENT = 18;

  MBEDTLS_ENTROPY_SOURCE_STRONG = 1;
  MBEDTLS_ENTROPY_SOURCE_WEAK = 0;

function mbedtls_ssl_close_notify(ssl: Pmbedtls_ssl_context): Integer; cdecl; external;
procedure mbedtls_ssl_free(ssl: Pmbedtls_ssl_context); cdecl; external;
function mbedtls_ssl_get_verify_result(ssl: Pmbedtls_ssl_context): uint32_t; cdecl; external;
function mbedtls_ssl_get_ciphersuite(const ssl: Pmbedtls_ssl_context): PChar; cdecl; external;
function mbedtls_ssl_get_version(const ssl: Pmbedtls_ssl_context): PChar; cdecl; external;
function mbedtls_ssl_get_max_frag_len(const ssl: Pmbedtls_ssl_context): size_t; cdecl; external;
function mbedtls_ssl_get_peer_cert(const ssl: Pmbedtls_ssl_context): Pmbedtls_x509_crt; cdecl; external;
function mbedtls_ssl_get_session(const ssl: Pmbedtls_ssl_context; session: Pmbedtls_ssl_session): Integer; cdecl; external;
function mbedtls_ssl_get_ciphersuite_name(const ciphersuite_id: Integer): PChar; cdecl; external;
function mbedtls_ssl_get_ciphersuite_id(const ciphersuite_name: PChar): Integer; cdecl; external;
function mbedtls_ssl_handshake(ssl: Pmbedtls_ssl_context): Integer; cdecl; external;
function mbedtls_ssl_handshake_client_step(ssl: Pmbedtls_ssl_context): Integer; cdecl; external;
procedure mbedtls_ssl_init(ssl: Pmbedtls_ssl_context); cdecl; external;
function mbedtls_ssl_list_ciphersuites: PInteger; cdecl; external;
function mbedtls_ssl_read(ssl: Pmbedtls_ssl_context; buf: Pointer; len: size_t): Integer; cdecl; external;
function mbedtls_ssl_set_hostname(ssl: Pmbedtls_ssl_context; hostname: PChar): Integer; cdecl; external;
procedure mbedtls_ssl_set_bio(ssl: Pmbedtls_ssl_context; p_bio: Pointer; f_send: TNetSendFunc; f_recv: TNetRecvFunc; f_recv_timeout: TNetRecvTimeoutFunc); cdecl; external;
procedure mbedtls_ssl_set_timer_cb(ssl: Pmbedtls_ssl_context; p_timer: Pointer; f_set_timer: TSetTimerFunc; f_get_timer: TGetTimerFunc); cdecl; external;
function mbedtls_ssl_setup(ssl: Pmbedtls_ssl_context; conf: Pmbedtls_ssl_config): Integer; cdecl; external;
function mbedtls_ssl_write(ssl: Pmbedtls_ssl_context; const buf: Pointer; len: size_t): Integer; cdecl; external;
function mbedtls_ssl_session_reset(ssl: Pmbedtls_ssl_context): Integer; cdecl; external;
function mbedtls_ssl_set_session(ssl: Pmbedtls_ssl_context; const session: Pmbedtls_ssl_session): Integer; cdecl; external;
function mbedtls_ssl_get_bytes_avail(ssl: Pmbedtls_ssl_context): size_t; cdecl; external;

procedure mbedtls_ssl_config_init(conf: Pmbedtls_ssl_config); cdecl; external;
function mbedtls_ssl_config_defaults(conf: Pmbedtls_ssl_config; endpoint: Integer; transport: Integer; preset: Integer): Integer; cdecl; external;
procedure mbedtls_ssl_config_free(conf: Pmbedtls_ssl_config); cdecl; external;

procedure mbedtls_ssl_conf_authmode(conf: Pmbedtls_ssl_config; authmode: Integer); cdecl; external;
procedure mbedtls_ssl_conf_ca_chain(conf: Pmbedtls_ssl_config; ca_chain: Pmbedtls_x509_crt; ca_crl: Pmbedtls_x509_crl); cdecl; external;
procedure mbedtls_ssl_conf_transport(conf: Pmbedtls_ssl_config; transport: Integer); cdecl; external;
procedure mbedtls_ssl_conf_rng(conf: Pmbedtls_ssl_config; f_rng: TrngFunc; p_rng: Pointer); cdecl; external;
procedure mbedtls_ssl_conf_dbg(conf: Pmbedtls_ssl_config; f_dbg: TdbgFunc; p_dbg: Pointer); cdecl; external; overload;
procedure mbedtls_ssl_conf_dbg(conf: Pmbedtls_ssl_config; f_dbg: Pointer; p_dbg: Pointer); cdecl; external; overload;
procedure mbedtls_ssl_conf_max_version(conf: Pmbedtls_ssl_config; major, minor: Integer); cdecl; external;
procedure mbedtls_ssl_conf_min_version(conf: Pmbedtls_ssl_config; major, minor: Integer); cdecl; external;
procedure mbedtls_ssl_conf_session_tickets(conf: Pmbedtls_ssl_config; use_tickets: Integer); cdecl; external;
procedure mbedtls_ssl_conf_read_timeout(ssl: Pmbedtls_ssl_context; timeout: uint32_t); cdecl; external;

function mbedtls_entropy_add_source(ctx: Pmbedtls_entropy_context; f_source: TEntropyFunc; p_source: Pointer; threshold: size_t; strong: Integer): Integer; cdecl; external;
procedure mbedtls_entropy_free(ctx: Pmbedtls_entropy_context); cdecl; external;
function mbedtls_entropy_func(Data: Pointer; output: PChar; len: size_t): Integer; cdecl; external;
procedure mbedtls_entropy_init(ctx: Pmbedtls_entropy_context); cdecl; external;

procedure mbedtls_ctr_drbg_init(ctx: Pmbedtls_ctr_drbg_context); cdecl; external;
function mbedtls_ctr_drbg_seed(ctx: Pmbedtls_ctr_drbg_context; f_entropy: TEntropyFunc; p_entropy: Pointer; custom: PChar; len: size_t): Integer; cdecl; external;
function mbedtls_ctr_drbg_random_with_add(p_rng: Pointer; output: PChar; output_len: size_t; additional: PChar; add_len: size_t): Integer; cdecl; external;
function mbedtls_ctr_drbg_random(p_rng: Pointer; output: PChar; output_len: size_t): Integer; cdecl; external;
procedure mbedtls_ctr_drbg_free(ctx: Pmbedtls_ctr_drbg_context); cdecl; external;

procedure mbedtls_debug_set_threshold(threshold: Integer); cdecl; external;

procedure mbedtls_x509_crt_init(crt: Pmbedtls_x509_crt); cdecl; external;
procedure mbedtls_x509_crt_free(crt: Pmbedtls_x509_crt); cdecl; external;
function mbedtls_x509_crt_parse(chain: Pmbedtls_x509_crt; buf: PChar; buflen: size_t): Integer; cdecl; external;
function mbedtls_x509_crt_parse_file(chain: Pmbedtls_x509_crt; const path: PChar): Integer; cdecl; external;
function mbedtls_x509_crt_parse_path(chain: Pmbedtls_x509_crt; const path: PChar): Integer; cdecl; external;
function mbedtls_x509_crt_verify_info(buf: PChar; size_: size_t; const prefix: PChar; flags: uint32_t): Integer; cdecl; external;

implementation

end.
